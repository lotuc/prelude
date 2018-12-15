(prelude-require-packages '(company-lsp
                            lsp-mode
                            lsp-ui))

(require 'lsp-mode)
(require 'lsp-ui)
(require 'company-lsp)
;; (require 'lsp-imenu)

(push 'company-lsp company-backends)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

(require 'lsp-clients)
(add-hook 'programming-mode-hook 'lsp)
(add-hook 'prog-major-mode #'lsp)

(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
(define-key lsp-ui-mode-map (kbd "C-c C-l .") 'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map (kbd "C-c C-l ?") 'lsp-ui-peek-find-references)
(define-key lsp-ui-mode-map (kbd "C-c C-l r") 'lsp-rename)
(define-key lsp-ui-mode-map (kbd "C-c C-l x") 'lsp-restart-workspace)
(define-key lsp-ui-mode-map (kbd "C-c C-l w") 'lsp-ui-peek-find-workspace-symbol)
(define-key lsp-ui-mode-map (kbd "C-c C-l i") 'lsp-ui-peek-find-implementation)
(define-key lsp-ui-mode-map (kbd "C-c C-l d") 'lsp-describe-thing-at-point)

(setq lsp-ui-sideline-enable t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-peek-enable t)
(setq lsp-ui-peek-always-show t)
;; (setq lsp-ui-sideline-ignore-duplicate t)

;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

;; dirty patch
;; https://github.com/emacs-lsp/lsp-mode/issues/400
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

(provide 'lotuc-lsp)
