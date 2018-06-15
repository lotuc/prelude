(prelude-require-packages '(autumn-light-theme
                            dracula-theme
                            solarized-theme
                            moe-theme
                            flatui-theme
                            flatland-theme
                            treemacs))
(require 'treemacs)
(require 'yasnippet)
(require 'prelude-helm-everywhere)

(global-set-key [f8] 'treemacs)
(setq treemacs-no-png-images t)
(treemacs-git-mode 'simple)
(yas-global-mode 1)

(provide 'personal-ui)
