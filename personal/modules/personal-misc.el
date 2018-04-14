(prelude-require-packages '(multiple-cursors
                            org-plus-contrib
                            elfeed
                            elfeed-org
                            docker
                            w3m
                            yasnippet-snippets
                            magithub
                            pdf-tools
                            auto-complete))
(require 'whitespace)
(require 'multiple-cursors)
(require 'elfeed)
(require 'magit)
(require 'pdf-tools)

(pdf-tools-install)

(defvar xsdvi-path (expand-file-name "~/.emacs.d/personal/java/xsdvi/xsdvi.jar"))
(defun lotuc-trans-xsd-to-svg (in-xsd-file out-svg-file)
  (interactive
   (list (read-file-name "XSD:")
         (expand-file-name (read-string "SVG Filename:")
                           (read-directory-name "Output Directory:"))))
  (let* ((out-dir
          (expand-file-name (make-temp-name "xsdvi") temporary-file-directory))
         (out-file-name
          (expand-file-name (format "%s.svg" (file-name-base in-xsd-file))
                            out-dir))
         (cmd (format "cd \"%s\" && java -jar \"%s\" \"%s\""
                      out-dir
                      (expand-file-name xsdvi-path)
                      (expand-file-name in-xsd-file))))
    (if (file-exists-p out-svg-file)
        (format "File already exists: %s" out-svg-file)
      (progn
        (make-directory out-dir)
        (if (and (equal (shell-command cmd "*xsdvi*") 0)
                 (file-exists-p out-file-name))
            (and (copy-file out-file-name out-svg-file) out-svg-file)
          (message (format "export xsd %s to svg %s" in-xsd-file out-svg-file)))))))

(defun lotuc-kill-other-buffers ()
  "Kill *all* other buffers, including special buffers.
crux-kill-other-buffers 'Doesn't mess with special buffers', we kill all others"
  (interactive)
  (when (y-or-n-p
         "Are you sure you want to kill *all* buffers but the current one? ")
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))))

;; https://github.com/bbatsov/prelude/issues/998
(defun fix-c-a ()
  (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-a") nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist)))
(add-hook 'eshell-mode-hook 'fix-c-a)
(add-hook 'haskell-interactive-mode-hook 'fix-c-a)

;; multiple cursor
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/edit-lines)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; elfeed
(global-set-key (kbd "C-x e") 'elfeed)
;; http://nullprogram.com/
;; solves "(28) Operation timeout."
(elfeed-set-timeout 60)
(elfeed-set-max-connections 2)
(defalias 'elfeed-toggle-star
  (elfeed-expose #'elfeed-search-toggle-all 'star))
(eval-after-load 'elfeed-search
  '((lambda ()
      (define-key elfeed-search-mode-map (kbd "m") 'elfeed-toggle-star)
      (define-key elfeed-search-mode-map (kbd "U") 'elfeed-update))))
(setq-default elfeed-search-filter "@7-day-ago +unread ")

(require 'elfeed-org)
(elfeed-org)
(let ((lotuc-subscribed-elfeed "~/.emacs.d/personal/private/elfeed.org"))
  (when (file-exists-p lotuc-subscribed-elfeed)
    (setq rmh-elfeed-org-files (list lotuc-subscribed-elfeed))))

(defface starred-elfeed-entry
  '((t :foreground "#e70"))
  "Marks an important Elfeed entry.")

(push '(star starred-elfeed-entry) elfeed-search-face-alist)

;; w3m
(global-set-key "\C-cm" 'w3m-browse-url)

;; whitespace-mode
(defun turn-off-whitespace-hook ()
  (whitespace-mode -1))
(add-hook 'gfm-mode-hook 'turn-off-whitespace-hook)
(add-hook 'elfeed-show-mode-hook 'turn-off-whitespace-hook)

;; https://magit.vc/manual/magit/Performance.html
(setq magit-refresh-status-buffer nil)

(setq line-number-display-limit large-file-warning-threshold)
(setq line-number-display-limit-width 200)
(defun my--is-file-large ()
  "If buffer too large and my cause performance issue."
  (< large-file-warning-threshold (buffer-size)))

(provide 'personal-misc)
