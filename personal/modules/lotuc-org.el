(prelude-require-packages '(org-plus-contrib
                            org-pomodoro
                            htmlize
                            ob-http
                            bbdb
                            bbdb-vcard
                            helm-bbdb
                            org-attach-screenshot
                            ox-hugo
                            org-download
                            helm-org-rifle))

;; https://emacs.stackexchange.com/questions/30559/org-mode-inline-image-display-size
(require 'org)
(require 'org-pomodoro)
(require 'org-capture)
(require 'ox-latex)
(require 'org-habit)
(require 'org-protocol)
(require 'org-clock)
(require 'bbdb)
(require 'bbdb-com)
(require 'org-crypt)
(require 'epa-file)
(require 'org-attach-screenshot)

(with-eval-after-load 'ox (require 'ox-hugo))

;;;; Screen capturing program
(when (equal system-type 'darwin)
  (setq org-attach-screenshot-command-line "screencapture -i %f"))

(setq org-modules
      '(org-bbdb
        org-bibtex
        org-crypt
        org-gnus
        org-id
        org-info
        org-habit
        org-inlinetask
        org-irc
        org-mew
        org-mhe
        org-protocol
        org-rmail
        org-vm
        org-wl
        org-w3m))

;;;; Variables
(defvar lotuc-jar-files-dir (expand-file-name "java" prelude-personal-dir)
  "This directory houses all the jar files")
(defvar org-plantuml-jar-path
  (expand-file-name "plantuml.jar" lotuc-jar-files-dir))
(defvar org-ditta-jar-path
  (expand-file-name "ditaa.jar" lotuc-jar-files-dir))
(defvar bh/hide-scheduled-and-waiting-next-tasks t)

;; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;;;; Encrypt
;; - https://gpgtools.org/
;; - gnupg
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-disable-auto-save nil)
;; GPG key to use for encryption
(setq org-crypt-key "936B445C")

;;;; Settings
;; https://emacs.stackexchange.com/questions/14535/how-can-i-use-helm-with-org-refile

;; slow
(setq org-startup-indented t)

(setq org-cycle-separator-lines 0)
(setq org-outline-path-complete-in-steps nil)
(setq org-src-preserve-indentation t)
(setq org-confirm-babel-evaluate nil)
(setq org-use-fast-todo-selection t)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-export-with-smart-quotes t)
(setq org-cycle-include-plain-lists t)
(setq org-hide-leading-stars t)
(setq org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
;; https://stackoverflow.com/questions/11670654/how-to-resize-images-in-org-mode
(setq org-image-actual-width nil)
(setq org-refile-use-outline-path t)
(setq org-refile-allow-creating-parent-nodes (quote confirm))
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-compact-blocks t)
(setq org-startup-with-inline-images nil)
(org-clock-persistence-insinuate)
(setq org-clock-history-length 23)
(setq org-clock-in-resume t)
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
(setq org-clock-into-drawer t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-out-when-done t)
(setq org-clock-persist t)
(setq org-clock-persist-query-resume nil)
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
(setq org-clock-report-include-clocking-task t)

(setq org-latex-remove-logfiles t)
(setq org-latex-logfiles-extensions
      (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm"
              "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl"
              "bbl")))

;; TODO settings
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)"
                        "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Capture
(setq org-capture-templates
      (quote (("t" "todo" entry (file org-default-notes-file)
               "* TODO %?\n%U\n%a\n"
               :clock-in t :clock-resume t)
              ("r" "respond" entry (file org-default-notes-file)
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
               :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file org-default-notes-file)
               "* %? :NOTE:\n%U\n%a\n"
               :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree org-default-notes-file)
               "* %?\n%U\n"
               :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file org-default-notes-file)
               "* TODO Review %c\n%U\n"
               :immediate-finish t)
              ("m" "Meeting" entry (file org-default-notes-file)
               "* MEETING with %? :MEETING:\n%U"
               :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file org-default-notes-file)
               "* PHONE %? :PHONE:\n%U"
               :clock-in t :clock-resume t)
              ("H" "Hugo post" entry (file org-default-notes-file)
               (function org-hugo-new-subtree-post-capture-template)
               :clock-in t :clock-resume t)
              ("h" "Habit" entry (file org-default-notes-file)
               (function org-habit-capture-template)
               :clock-in t :clock-resume t))))

;;;; Keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "C-<f12>") 'org-pomodoro)
(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)
(global-set-key (kbd "<f9> v") 'org-toggle-inline-images)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> l") 'org-toggle-link-display)
(global-set-key (kbd "<f9> n") 'bh/toggle-next-task-display)
(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> 1") 'org-attach-screenshot)
(global-set-key (kbd "<f9> b") 'helm-bbdb)
(global-set-key (kbd "<f9> 2") 'lotuc/insert-contact)
(global-set-key (kbd "<f9> @") 'lotuc/insert-contact)
(global-set-key (kbd "<f12>") 'bh/show-org-agenda)



(defun org-hugo-new-subtree-post-capture-template ()
  "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
  (let* ((title (read-from-minibuffer "Post Title: "))
         (fname (org-hugo-slug title)))
    (mapconcat #'identity
               `(
                 ,(concat "* TODO " title)
                 ":PROPERTIES:"
                 ,(concat ":EXPORT_FILE_NAME: " fname)
                 ":END:"
                 "%U\n%?")          ;Place the cursor here finally
               "\n")))

(defun org-habit-capture-template ()
  (concat
   "* NEXT %?\n%U\n%a"
   "\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")"
   "\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))
;; Agenda
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy '(category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header
                             (concat "Project Next Tasks"
                                     (if bh/hide-scheduled-and-waiting-next-tasks
                                         ""
                                       " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function
                             'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled
                             bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines
                             bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date
                             bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header
                             (concat "Project Subtasks"
                                     (if bh/hide-scheduled-and-waiting-next-tasks
                                         ""
                                       " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function
                             'bh/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled
                             bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines
                             bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date
                             bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header
                             (concat "Standalone Tasks"
                                     (if bh/hide-scheduled-and-waiting-next-tasks
                                         ""
                                       " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled
                             bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines
                             bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date
                             bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy '(category-keep))))
                (tags-todo "-CANCELLED+WAITING|HOLD/!"
                           ((org-agenda-overriding-header
                             (concat "Waiting and Postponed Tasks"
                                     (if bh/hide-scheduled-and-waiting-next-tasks
                                         ""
                                       " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled
                             bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines
                             bh/hide-scheduled-and-waiting-next-tasks)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil))))

(defun bh/org-auto-exclude-function (tag)
  "Agenda 执行 =/ RET= 时的默认过滤条件，执行 =//= 取消过滤"
  (and (cond
        ((string= tag "hold") t)
        ((string= tag "habit") t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

;;;; babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (ruby . t)
   (org . t)
   (ditaa . t)
   (latex . t)
   (dot . t)
   (plantuml . t)
   (emacs-lisp . t)
   (ledger . t)
   (gnuplot . t)
   (screen . nil)
   (shell . t)
   (python . t)
   (java . t)
   (lilypond . t)
   (js . t)
   (sql . nil)
   (sqlite . t)
   (http . t)))

;;;; export
(setq org-latex-remove-logfiles t)
(setq org-latex-logfiles-extensions
      '("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi"
        "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"))

;;;; TeX
;; Use XeLaTeX to export PDF in Org-mode
(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))

;; 删除段落中多余空格（段落中中文折行出导出将产生空格）
(defun paragraph-filter-delete-extra-space (data backend info)
  (setq data (replace-regexp-in-string
              (concat "\\(\\cc\\)\n\\(\\cc\\)") "\\1\\2" data)))
(with-eval-after-load 'ox
  (add-to-list 'org-export-filter-paragraph-functions
               'paragraph-filter-delete-extra-space))

;;;; Functions

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun lotuc/org-attach-insert ()
  "Insert attach path at current point"
  (interactive)
  (let* ((attach-dir (org-attach-dir t))
         (files (org-attach-file-list attach-dir))
         (file (if (= (length files) 1)
                   (car files)
                 (completing-read "Open attachment: "
                                  (mapcar #'list files) nil t)))
         (path (expand-file-name file attach-dir))
         (file-path (file-relative-name path default-directory)))
    (org-attach-annex-get-maybe path)
    (insert (concat "[[file:" file-path "]]"))))

(defun lotuc/org-columns-insert-dblock ()
  "Copy from org-colview.el. difference is that we enable :indent by default"
  (interactive)
  (let ((id (completing-read
             "Capture columns (local, global, entry with :ID: property) [local]: "
             (append '(("local") ("global"))
                     (mapcar #'list (org-property-values "ID"))))))
    (org-create-dblock
     (list :name "columnview"
           :hlines 1
           :indent 1
           :id (cond ((string= id "global") 'global)
                     ((member id '("" "local")) 'local)
                     (id)))))
  (org-update-dblock))

;; Agenda Functions
(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks
        (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks"
           (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next)
                          (< (point) subtree-end)
                          (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p) subtree-end)
       ((org-is-habit-p) subtree-end)
       ((bh/is-project-subtree-p) subtree-end)
       (t nil)))))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p) nil)
       (t next-headline)))))

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-number
                             (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string
                                  "%Y-%m-"
                                  (time-subtract
                                   (current-time)
                                   (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current
                      (save-excursion
                        (forward-line 1)
                        (and (< (point) subtree-end)
                             (re-search-forward
                              (concat last-month "\\|" this-month)
                              subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))


;;;; Speed commands
(setq org-use-speed-commands t)
(setq org-speed-commands-user
      (quote (("d" . org-decrypt-entry)
              ("e" . org-encrypt-entry)
              ("h" . bh/hide-other)
              ("q" . bh/show-org-agenda)
              ("s" . org-save-all-org-buffers)
              ("w" . org-refile)
              ("z" . org-add-note)

              ("J" . org-clock-goto)
              ("N" . bh/narrow-to-org-subtree)
              ("P" . bh/narrow-to-org-project)
              ("T" . bh/org-todo)
              ("W" . bh/widen))))

;; Speed functions
(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (outline-hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/show-org-agenda ()
  (interactive)
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda( )*")
    (switch-to-buffer "*Org Agenda*")))

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

;;;; Working process: clocking
(defvar bh/keep-clock-running nil)
(defvar bh/organization-task-id "93861DA4-319F-411A-94F4-86B2FAE661BA")

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))


(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;; We're in the agenda
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;; We are not in the agenda
    (save-restriction
      (widen)
      ;; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

;;;; Estimate
;; global Effort estimate values
;; global STYLE property values for completion
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
(setq org-global-properties
      (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
              ("STYLE_ALL" . "habit"))))

;;;; Personal hooks
(defun lotuc/org-mode-hook ()
  (progn
    (dolist (face '(org-level-1
                    org-level-2
                    org-level-3
                    org-level-4
                    org-level-5))
      (set-face-attribute face nil :weight 'semi-bold :height 1.0))
    (local-set-key (kbd "C-c C-x i") 'lotuc/org-columns-insert-dblock)
    (local-set-key (kbd "<f9> a") 'lotuc/org-attach-insert)))

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(add-hook 'org-mode-hook 'lotuc/org-mode-hook)
(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)
(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;; https://github.com/caiorss/org-wiki
(require 'org-wiki)
(defalias 'wiki #'org-wiki-switch-root)
(defalias 'wiki/root #'org-wiki-switch-root)
(defalias 'wiki/helm #'org-wiki-helm)
(defalias 'wiki/insert #'org-wiki-index)
(defalias 'wiki/close #'org-wiki-close)
(defalias 'wiki/new #'org-wiki-insert-new)

(provide 'lotuc-org)
