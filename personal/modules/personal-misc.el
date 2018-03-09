(prelude-require-packages '(multiple-cursors
                            org-plus-contrib
                            magit
                            elfeed
                            elfeed-org
                            docker
                            w3m))
(require 'whitespace)
(require 'multiple-cursors)
(require 'elfeed)
(require 'magit)

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

(defun list-git-repo-in-directory-recursively (dir dep)
  "find git repo under directory recursively with within the depth"
  (defun is-git-repo (d)
    "silly function check if the directory is a git repo"
    (file-directory-p (expand-file-name ".git" d)))
  (defun helper (dir dep)
    "the real finding function"
    (if (< dep 1)
        '()
      (let* ((file-list (seq-map (lambda (n) (expand-file-name n dir))
                                 (seq-filter (lambda (n)
                                               (not (or (equal n ".")
                                                        (equal n "..")
                                                        (equal n ".git"))))
                                             (directory-files dir))))
             (sub-dir-list (seq-filter #'file-directory-p file-list))
             ;; Git Repo at child of *dir*
             (child-repo-list
              (seq-filter #'is-git-repo sub-dir-list))
             (sub-not-repo-dir-list (seq-filter
                                     (lambda (d) (not (is-git-repo d)))
                                     sub-dir-list))
             ;; Git Repo at child of child dir and their child's and so on
             (grand-repo-list
              (apply #'append
                     (seq-map (lambda (d) (helper d (- dep 1)))
                              sub-not-repo-dir-list))))
        (append child-repo-list grand-repo-list))))
  (helper dir dep))

(defun lotuc-magit ()
  "quick magit on repos under my workspace"
  (interactive)
  (let ((repo (completing-read
               "Repo: "
               (append
                '("~/.emacs.d")
                (list-git-repo-in-directory-recursively "~/Workspace/" 5)))))
    (magit-status-internal repo)))


(setq line-number-display-limit large-file-warning-threshold)
(setq line-number-display-limit-width 200)
(defun my--is-file-large ()
  "If buffer too large and my cause performance issue."
  (< large-file-warning-threshold (buffer-size)))

(define-derived-mode my-large-file-mode fundamental-mode "LargeFile"
  "Fixes performance issues in Emacs for large files."
  ;; (setq buffer-read-only t)
  (setq bidi-display-reordering nil)
  (jit-lock-mode nil)
  (buffer-disable-undo)
  (set (make-variable-buffer-local 'global-hl-line-mode) nil)
  (set (make-variable-buffer-local 'line-number-mode) nil)
  (set (make-variable-buffer-local 'column-number-mode) nil) )

(add-to-list 'magic-mode-alist (cons #'my--is-file-large #'my-large-file-mode))

;; (add-hook 'find-file-hook
;;           (when (> (buffer-size) (* 1024 1024))
;;             (setq buffer-read-only t)
;;             (buffer-disable-undo)))

(provide 'personal-misc)
