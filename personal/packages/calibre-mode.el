;; https://github.com/whacked/calibre-mode
(require 'cl)
(require 'org)

(defvar sql-sqlite-program "/usr/bin/sqlite3")
(defvar calibre-root-dir (expand-file-name "~/Calibre Library"))
(defvar calibre-db (concat calibre-root-dir "/metadata.db"))

(defun calibre-chomp (s)
  (replace-regexp-in-string "[\s\n]+$" "" s))

(defun calibre-query (sql-query)
  (shell-command-to-string
   (format "%s -separator '\t' '%s' '%s'" sql-sqlite-program calibre-db sql-query)))

(defun calibre-query-to-alist (query-result)
  "builds alist out of a full calibre-query query record result"
  (if query-result
      (let ((spl-query-result (split-string (calibre-chomp query-result) "\t")))
        `((:id                     ,(nth 0 spl-query-result))
          (:author-sort            ,(nth 1 spl-query-result))
          (:book-dir               ,(nth 2 spl-query-result))
          (:book-name              ,(nth 3 spl-query-result))
          (:book-format  ,(downcase (nth 4 spl-query-result)))
          (:book-pubdate           ,(nth 5 spl-query-result))
          (:book-title             ,(nth 6 spl-query-result))
          (:file-path    ,(concat (file-name-as-directory calibre-root-dir)
                                  (file-name-as-directory (nth 2 spl-query-result))
                                  (nth 3 spl-query-result) "."
                                  (downcase (nth 4 spl-query-result))))))))

(defun calibre-build-default-query (whereclause &optional limit)
  (concat "SELECT "
          "b.id, b.author_sort, b.path, d.name, d.format, b.pubdate, b.title"
          " FROM data AS d "
          "LEFT OUTER JOIN books AS b ON d.book = b.id "
          whereclause
          (when limit
            (format "LIMIT %s" limit))))

(defun calibre-query-build-like (fieldname argstring)
  (format "LOWER(%s) LIKE '\\''%%%s%%'\\''" fieldname (downcase argstring)))

(defun calibre-query-build-like-author (argstring)
  (calibre-query-build-like "b.author_sort" argstring))

(defun calibre-query-build-like-title (argstring)
  (calibre-query-build-like "b.title" argstring))

(defun calibre-query-by-field (wherefield argstring)
  (format "WHERE %s" (calibre-query-build-like wherefield argstring)))

(defun calibre-parse-query-string (search-string)
    (interactive)
    "a:<author_name> | t:<title_name> | <author_or_title> | <query-1> & <query-2>"
    (defun like (column s)
      (format "LOWER(%s) LIKE '\\''%%%s%%'\\''" column (downcase s)))
    (defun fields-like (s) (format "%s OR %s"
                                   (calibre-query-build-like-author s)
                                   (calibre-query-build-like-title s)))
    (defun parse-part (part)
      (let* ((spl-arg (split-string part ":"))
             (argname (string-trim (car spl-arg)))
             (argstring (if (= 1 (length spl-arg))
                            nil
                          (string-trim (string-join (cdr spl-arg) ":")))))
        (cond ((not argstring) (fields-like part))
              ((or (string= "a" argname)
                   (string= "author" argname))
               (calibre-query-build-like-author argstring))
              ((or (string= "t" argname)
                   (string= "title" argname))
               (calibre-query-build-like-title argstring))
              ;; default searching
              (t (fields-like part)))))
    (let* ((parts (split-string search-string "&"))
           (where-parts (mapcar #'parse-part parts)))
      (format "WHERE %s" (string-join where-parts " AND "))))

(defun calibre-read-query-filter-command ()
  (let* ((default-string (if mark-active (calibre-chomp (buffer-substring (mark) (point)))))
         ;; prompt &optional initial keymap read history default
         (search-string (read-string
                         (format "search string[ %s ]: "
                                 (if default-string default-string
                                   "a:<author_name> | t:<title_name> | <author_or_title> | <query-1> & <query-2>"))
                                     nil nil default-string)))
    (calibre-parse-query-string search-string)))

(defun quote-% (str)
  (replace-regexp-in-string "%" "%%" str))

(defun getattr (my-alist key)
  (cadr (assoc key my-alist)))

(defun mark-aware-copy-insert (content)
  "copy to clipboard if mark active, else insert"
  (if mark-active
      (progn (kill-new content)
             (deactivate-mark))
    (insert content)))

;; define the result handlers here in the form of (hotkey description handler-function)
;; where handler-function takes 1 alist argument containing the result record
(defvar calibre-handler-alist
  '(("o" "open with emacs"
     (lambda (res) (org-open-file-with-emacs (getattr res :file-path))))
    ("O" "open with system"
     (lambda (res) (org-open-file-with-system (getattr res :file-path))))
    ("x" "open with xournal"
     (lambda (res) (start-process "xournal-process" "*Messages*" "xournal"
                                  (let ((xoj-file-path (concat calibre-root-dir "/" (getattr res :book-dir) "/" (getattr res :book-name) ".xoj")))
                                    (if (file-exists-p xoj-file-path)
                                        xoj-file-path
                                      (getattr res :file-path))))))
    ("s" "insert calibre search string"
     (lambda (res) (mark-aware-copy-insert (concat "title:\"" (getattr res :book-title) "\""))))
    ("i" "get book information (SELECT IN NEXT MENU) and insert"
     (lambda (res)
       (let ((opr (char-to-string (read-char
                                   ;; render menu text here
                                   (concat "What information do you want?\n"
                                           "i : values in the book's `Ids` field (ISBN, DOI...)\n"
                                           "d : pubdate\n"
                                           "a : author list\n")))))
         (cond ((string= "i" opr)
                ;; stupidly just insert the plain text result
                (mark-aware-copy-insert
                 (calibre-chomp
                  (calibre-query (concat "SELECT "
                                         "idf.type, idf.val "
                                         "FROM identifiers AS idf "
                                         (format "WHERE book = %s" (getattr res :id)))))))
               ((string= "d" opr)
                (mark-aware-copy-insert
                 (substring (getattr res :book-pubdate) 0 10)))
               ((string= "a" opr)
                (mark-aware-copy-insert
                 (calibre-chomp (getattr res :author-sort))))
               (t
                (deactivate-mark)
                (message "cancelled"))))))
    ("p" "insert file path"
     (lambda (res) (mark-aware-copy-insert (getattr res :file-path))))
    ("P" "insert org file path"
     (lambda (res) (mark-aware-copy-insert (format "[[calibre:a:%s & t:%s][%s]]"
                                                   (getattr res :author-sort)
                                                   (getattr res :book-title)
                                                   (getattr res :book-title)))))
    ("t" "insert title"
     (lambda (res) (mark-aware-copy-insert (getattr res :book-title))))
    ("j" "insert entry json"
     (lambda (res) (mark-aware-copy-insert (json-encode res))))
    ("q" "(or anything else) to cancel"
     (lambda (res)
       (deactivate-mark)
       (message "cancelled")))))

(defun calibre-file-interaction-menu (calibre-item)
  (if (file-exists-p (getattr calibre-item :file-path))
      (let ((opr (char-to-string (read-char
                                  ;; render menu text here
                                  (concat (format "(%s) [%s] found, what do?\n"
                                                  (getattr calibre-item :book-format)
                                                  (getattr calibre-item :book-name))
                                          (mapconcat #'(lambda (handler-list)
                                                         (let ((hotkey      (elt handler-list 0))
                                                               (description (elt handler-list 1))
                                                               (handler-fn  (elt handler-list 2)))
                                                           ;; ULGY BANDAID HACK
                                                           ;; replace "insert" with "copy to clipboard" if mark-active
                                                           (format " %s :   %s"
                                                                   hotkey
                                                                   (if mark-active
                                                                       (replace-regexp-in-string "insert \\(.*\\)" "copy \\1 to clipboard" description)
                                                                     description)))
                                                         ) calibre-handler-alist "\n"))))))
        (funcall
         (elt (if (null (assoc opr calibre-handler-alist)) (assoc "q" calibre-handler-alist)
                (assoc opr calibre-handler-alist)) 2) calibre-item))
    (message "didn't find that file")))

(defun calibre-format-selector-menu (calibre-item-list)
  (let* ((chosen-number
          (char-to-string
           (read-char
            ;; render menu text here
            (let ((num-result (length calibre-item-list)))
              (concat (format "%d matches, choose one\n" num-result)
                      ;; (getattr (car calibre-item-list) :book-title)
                      (mapconcat #'(lambda (idx)
                                     (let ((item (nth idx calibre-item-list)))
                                       (format "   (%s) %s"
                                               (1+ idx)
                                               (getattr item :book-title))))
                                 (number-sequence 0 (1- num-result))
                                 "\n"))))))
         (chosen-item (nth (1- (string-to-number chosen-number)) calibre-item-list)))
    (calibre-file-interaction-menu chosen-item)))

(defun calibre-find (&optional custom-query)
  (interactive)
  (let* ((sql-query (if custom-query
                        custom-query
                      (calibre-build-default-query (calibre-read-query-filter-command))))
         (query-result (calibre-query sql-query))
         (line-list (if (string= query-result "")
                        '()
                      (split-string (calibre-chomp query-result) "\n")))
         (num-result (length line-list)))
    (if (= 0 num-result)
        (progn
          (message "nothing found.")
          (deactivate-mark))
      (let ((res-list (mapcar #'(lambda (line) (calibre-query-to-alist line)) line-list)))
        (if (= 1 (length res-list))
            (calibre-file-interaction-menu (car res-list))
          (calibre-format-selector-menu res-list))))))

(defun lotuc/calibre-get-path (search-string)
  (interactive)
  (let* ((pths (mapcar
                 (lambda (alist) (getattr alist :file-path))
                 (mapcar
                  #'calibre-query-to-alist
                  (split-string
                   (calibre-chomp
                    (calibre-query (calibre-build-default-query
                                    (calibre-parse-query-string search-string))))
                   "\n"))))
         (count (length pths)))
    (cond
     ((= count 1) (car pths))
     ((= count 0) (progn (message
                          (format "calibre-get-path no book found: %s"
                                  search-string))
                         nil))
     (t (progn (message (format "calibre-get-path found multiple(%d): %s"
                                count search-string))
               (car pths))))))

(global-set-key "\C-cC" 'calibre-find)

(provide 'calibre-mode)
