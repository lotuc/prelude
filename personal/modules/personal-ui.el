(prelude-require-packages '(autumn-light-theme
                            dracula-theme
                            solarized-theme
                            moe-theme
                            flatui-theme
                            flatland-theme
                            neotree
                            ;; treemacs
                            ))
(require 'yasnippet)
(require 'prelude-helm-everywhere)

;; (require 'treemacs)
;; (global-set-key [f8] 'treemacs)
;; (setq treemacs-no-png-images t)
;; (treemacs-git-mode 'simple)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(yas-global-mode 1)

(provide 'personal-ui)
