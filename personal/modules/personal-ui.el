(prelude-require-packages '(autumn-light-theme
                            dracula-theme
                            solarized-theme
                            moe-theme
                            flatui-theme
                            flatland-theme
                            neotree
                            all-the-icons
                            ;; treemacs
                            spaceline
                            spaceline-all-the-icons
                            ))
(require 'yasnippet)
(require 'prelude-helm-everywhere)
(require 'spaceline-config)
(require 'spaceline-all-the-icons)

;; (require 'treemacs)
;; (global-set-key [f8] 'treemacs)
;; (setq treemacs-no-png-images t)
;; (treemacs-git-mode 'simple)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(global-set-key [C-f8] 'neotree-projectile-action)
(setq projectile-use-git-grep 1)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(spaceline-emacs-theme)
(spaceline-helm-mode)
;; (spaceline-all-the-icons-theme)
;; (setq inhibit-compacting-font-caches t)

(yas-global-mode 1)

(provide 'personal-ui)
