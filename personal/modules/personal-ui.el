(prelude-require-packages '(autumn-light-theme
                            dracula-theme
                            solarized-theme
                            treemacs
                            multiple-cursors
                            eshell-prompt-extras
                            elfeed
                            elfeed-org))
(require 'treemacs)
(require 'multiple-cursors)
(require 'eshell-prompt-extras)
(require 'elfeed)

(require 'prelude-helm-everywhere)
(require 'whitespace)

;;;; disable flycheck-mode https://github.com/bbatsov/prelude/issues/508
;; (global-flycheck-mode -1)
;; (defun disable-flycheck-mode ()
;;   (interactive)
;;   (flycheck-mode -1))
;; (add-hook 'prog-mode-hook 'disable-flycheck-mode)


(global-set-key [f8] 'treemacs-toggle)
(setq treemacs-no-png-images t)

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

;; elshell-prompt-extras
(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-dakrone "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-dakrone))

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

;; whitespace-mode
(defun turn-off-whitespace-hook ()
  (whitespace-mode -1))
(add-hook 'org-mode-hook 'turn-off-whitespace-hook)
(add-hook 'gfm-mode-hook 'turn-off-whitespace-hook)

;; misc
(defun eshell/c ()
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))
;; clear Welcome message
(add-hook 'eshell-mode-hook 'eshell/c)

;; crux-kill-other-buffers saves **
;;;###autoload
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(provide 'personal-ui)
