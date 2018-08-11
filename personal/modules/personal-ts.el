(prelude-require-packages '(lsp-mode
                            lsp-javascript-typescript
                            typescript-mode
                            prettier-js))
(require 'prelude-programming)
(require 'company)
(require 'prettier-js)
(require 'lsp-javascript-typescript)

(defun personal-js-ts-defaults ()
  (setq typescript-indent-level 2)
  (setq js-indent-level 2)
  (setq prettier-js-args '(
                           "--single-quote"
                           "--trailing-comma" "none"
                           "--bracket-spacing" "true"))
  (lsp-javascript-typescript-enable)
  ;; The prettier is pretty slow :(
  ;; (add-hook 'after-save-hook 'prettier-js nil 'local)
  )

(defun personal-css-defaults ()
  (add-hook 'after-save-hook 'prettier-js nil 'local))

(add-hook 'js-mode-hook 'personal-js-ts-defaults)
(add-hook 'typescript-mode-hook 'personal-js-ts-defaults)
(add-hook 'js3-mode-hook 'personal-js-ts-defaults)
(add-hook 'rjsx-mode 'personal-js-ts-defaults)
(eval-after-load 'js-mode
  '(progn
     (defun my-company-transformer (candidates)
       (let ((completion-ignore-case t))
         (all-completions (company-grab-symbol) candidates)))
     (defun my-js-hook nil
       (make-local-variable 'company-transformers)
       (push 'my-company-transformer company-transformers))
     (add-hook 'js-mode-hook 'my-js-hook)))

(add-hook 'css-mode-hook 'personal-css-defaults)
(add-hook 'scss-mode-hook 'personal-css-defaults)

(provide 'personal-ts)
