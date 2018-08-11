(prelude-require-packages '(lsp-mode
                            lsp-ui
                            company-lsp
                            lsp-javascript-typescript))

;; (require 'lsp-mode)
;; (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable)

;; (require 'lsp-typescript)

;; ;; TypeScript
;; ;; npm i -g javascript-typescript-langserver
;; (add-hook 'js-mode-hook #'lsp-typescript-enable)
;; (add-hook 'js2-mode-hook #'lsp-typescript-enable) ;; for js2-mode support
;; (add-hook 'rjsx-mode #'lsp-typescript-enable) ;; for rjsx-mode support
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;               (lsp-typescript-enable))))


(require 'lsp-imenu)
;; (require 'company-lsp)
(add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
;; (push 'company-lsp company-backends)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(provide 'personal-lsp)
