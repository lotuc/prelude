(prelude-require-packages '(lsp-mode
                            lsp-ui
                            company-lsp))

(require 'lsp-mode)
(require 'lsp-imenu)
(require 'company-lsp)
(require 'lsp-ui)

(setq lsp-ui-sideline-ignore-duplicate t)
(add-hook 'prog-major-mode #'lsp-prog-major-mode-enable)
(add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
(push 'company-lsp company-backends)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(provide 'personal-lsp)
