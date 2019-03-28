(prelude-require-packages '(
                            autumn-light-theme
                            dracula-theme
                            solarized-theme
                            moe-theme
                            flatui-theme
                            flatland-theme
                            gruvbox-theme
                            neotree
                            all-the-icons
                            spaceline
                            spaceline-all-the-icons
                            multiple-cursors))
(require 'yasnippet)
(require 'multiple-cursors)
(require 'spaceline-config)
(require 'spaceline-all-the-icons)
(require 'prelude-helm-everywhere)

;; ace-window
;; https://github.com/Alexander-Miller/treemacs/issues/301
;; (with-eval-after-load "treemacs"
;;   (setq aw-ignored-buffers (delete 'treemacs-mode aw-ignored-buffers)))

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(global-set-key [C-f8] 'neotree-projectile-action)
(setq neo-window-fixed-size nil)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq projectile-use-git-grep 1)

(spaceline-emacs-theme)
(spaceline-helm-mode)

(yas-global-mode 1)

(when (eq system-type 'darwin)
  (progn
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)))

;; multiple cursor
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/edit-lines)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(provide 'lotuc-ui)
