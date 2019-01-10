(prelude-require-packages '(
                            autumn-light-theme
                            dracula-theme
                            solarized-theme
                            moe-theme
                            flatui-theme
                            flatland-theme
                            gruvbox-theme
                            ;; neotree
                            treemacs
                            treemacs-projectile
                            all-the-icons
                            spaceline
                            spaceline-all-the-icons
                            multiple-cursors))
(require 'yasnippet)
(require 'multiple-cursors)
(require 'spaceline-config)
(require 'spaceline-all-the-icons)
(require 'prelude-helm-everywhere)

(global-set-key [f8] 'treemacs)
(global-set-key [C-f8] 'treemacs-projectile)
(treemacs-resize-icons 44)
(treemacs-follow-mode t)
;; ace-window
;; https://github.com/Alexander-Miller/treemacs/issues/301
(setq aw-ignored-buffers (delete 'treemacs-mode aw-ignored-buffers))
(setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
      treemacs-deferred-git-apply-delay   0.5
      treemacs-display-in-side-window     t
      treemacs-file-event-delay           5000
      treemacs-file-follow-delay          0.2
      treemacs-follow-after-init          t
      treemacs-follow-recenter-distance   0.1
      treemacs-git-command-pipe           ""
      treemacs-goto-tag-strategy          'refetch-index
      treemacs-indentation                1
      treemacs-indentation-string         " "
      treemacs-is-never-other-window      nil
      treemacs-max-git-entries            5000
      treemacs-no-png-images t
      treemacs-no-delete-other-windows    t
      treemacs-project-follow-cleanup     nil
      treemacs-persist-file              (expand-file-name
                                          ".cache/treemacs-persist"
                                          user-emacs-directory)
      treemacs-recenter-after-file-follow nil
      treemacs-recenter-after-tag-follow  nil
      treemacs-show-cursor                nil
      treemacs-show-hidden-files          t
      treemacs-silent-filewatch           nil
      treemacs-silent-refresh             nil
      treemacs-sorting                    'alphabetic-desc
      treemacs-space-between-root-nodes   t
      treemacs-tag-follow-cleanup         t
      treemacs-tag-follow-delay           1.5
      treemacs-width                      20)

;; (require 'neotree)
;; (global-set-key [f8] 'neotree-toggle)
;; (global-set-key [C-f8] 'neotree-projectile-action)
;; (setq neo-window-fixed-size nil)
;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
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
