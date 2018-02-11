(prelude-require-packages '(autumn-light-theme
                            dracula-theme
                            solarized-theme
                            moe-theme
                            treemacs))
(require 'treemacs)
(require 'prelude-helm-everywhere)

;;;; disable flycheck-mode https://github.com/bbatsov/prelude/issues/508
;; (global-flycheck-mode -1)
;; (defun disable-flycheck-mode ()
;;   (interactive)
;;   (flycheck-mode -1))
;; (add-hook 'prog-mode-hook 'disable-flycheck-mode)

(global-set-key [f8] 'treemacs-toggle)
(setq treemacs-no-png-images t)

(provide 'personal-ui)
