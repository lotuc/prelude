(prelude-require-packages '(tide
                            typescript-mode
                            web-mode
                            company))

(require 'web-mode)
(require 'typescript-mode)
(require 'company)
(require 'flycheck)
(require 'tide)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  ;; This tide-format is not very beautiful
  (add-hook 'before-save-hook 'tide-format-before-save)
  (setq company-tooltip-align-annotations t))

(add-hook 'typescript-mode-hook
          (lambda ()
            (setup-tide-mode)
            (setq typescript-indent-level 2)))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode)
              (flycheck-add-mode 'typescript-tslint 'web-mode)
              (setq web-mode-markup-indent-offset 2)
              (setq web-mode-code-indent-offset 2)
              (setq web-mode-css-indent-offset 2))))

(provide 'personal-ts)
