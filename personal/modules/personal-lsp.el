(prelude-require-packages '(lsp-mode
                            lsp-ui
                            company-lsp))

(require 'lsp-mode)
(require 'lsp-imenu)
(require 'company-lsp)
(require 'lsp-ui)

(defun lsp-format-buffer ()
  "Ask the server to format this document."
  (interactive "*")
  (unless (or (lsp--capability "documentFormattingProvider")
              (lsp--registered-capability "textDocument/formatting"))
    (signal 'lsp-capability-not-supported (list "documentFormattingProvider")))
  (let ((edits (lsp--send-request (lsp--make-request
                                   "textDocument/formatting"
                                   (lsp--make-document-formatting-params)))))
    (let ((point (point))
          (w-start (window-start)))
      (lsp--apply-text-edits edits)
      (goto-char point)
      (goto-char (line-beginning-position))
      (set-window-start (selected-window) w-start))))

(setq lsp-ui-sideline-ignore-duplicate t)
(add-hook 'prog-major-mode #'lsp-prog-major-mode-enable)
(add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
(push 'company-lsp company-backends)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(provide 'personal-lsp)
