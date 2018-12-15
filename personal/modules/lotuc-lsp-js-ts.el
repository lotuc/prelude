(prelude-require-packages '(lsp-mode
                            lsp-javascript-typescript
                            ;; lsp-typescript
                            typescript-mode
                            prettier-js
                            web-mode))
(require 'prelude-programming)
(require 'company)
(require 'prettier-js)
;; (require 'lsp-javascript-typescript)
;; (require 'lsp-typescript)

(defun lotuc/js-ts-defaults ()
  ;; (lsp-javascript-typescript-enable)

  ;; typescript-language-server: dianostic-queue.js # updateDianostics
  ;; https://www.tslang.cn/docs/handbook/error.html
  ;; diagnostics.set(kind, event.body.diagnostics
  ;;   .filter(d => [8002, 8003, 8004, 8005, 8006, 8007, 8008, 8009, 8010,
  ;;               8011, 8012, 8013, 8014, 8015, 8016].indexOf(d.code) === -1)
  ;;   .map(protocol_translation_1.toDiagnostic));
  ;; (lsp-typescript-enable)
  (setq typescript-indent-level 2)
  (setq js-indent-level 2)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq prettier-js-args '("--print-width" "100"
                           "--single-quote" "true"
                           "--trailing-comma" "none"
                           "--bracket-spacing" "true"))

  ;; lsp-ui flychecker is not usable for ts now
  ;; (let ((ext (file-name-extension buffer-file-name)))
  ;;   (when (or ;; (string-equal "tsx" ext)
  ;;             (string-equal "ts" ext))
  ;;     (flycheck-select-checker 'typescript-tslint)))
  )

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (let ((ext (file-name-extension buffer-file-name)))
              (when (or (string-equal "tsx" ext)
                        (string-equal "jsx" ext))
                (lotuc/js-ts-defaults)))))

(defun lotuc/css-defaults ()
  (add-hook 'after-save-hook 'prettier-js nil 'local))

(add-hook 'js2-mode-hook 'lotuc/js-ts-defaults)
(add-hook 'typescript-mode-hook 'lotuc/js-ts-defaults)
(add-hook 'js3-mode-hook 'lotuc/js-ts-defaults)
(add-hook 'rjsx-mode 'lotuc/js-ts-defaults)
(eval-after-load 'js-mode
  '(progn
     (defun my-company-transformer (candidates)
       (let ((completion-ignore-case t))
         (all-completions (company-grab-symbol) candidates)))
     (defun my-js-hook ()
       (make-local-variable 'company-transformers)
       (push 'my-company-transformer company-transformers))
     (defun my-json-hook ()
       (make-local-variable 'js-indent-level)
       (setq js-indent-level 2))
     (add-hook 'json-mode-hook 'my-json-hook)
     (add-hook 'js-mode-hook 'my-js-hook)))

(add-hook 'css-mode-hook 'lotuc/css-defaults)
(add-hook 'scss-mode-hook 'lotuc/css-defaults)

(provide 'lotuc-lsp-js-ts)
