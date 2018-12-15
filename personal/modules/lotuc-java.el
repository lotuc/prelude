(prelude-require-packages '(lsp-mode
                            lsp-java))

(add-hook 'java-mode-hook #'lsp-java-enable)
(add-hook 'java-mode-hook 'lsp-java-enable)
(add-hook 'java-mode-hook 'flycheck-mode)
(add-hook 'java-mode-hook 'company-mode)
(add-hook 'java-mode-hook 'lsp-ui-mode)

(provide 'lotuc-java)
