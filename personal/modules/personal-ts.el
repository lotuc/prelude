(prelude-require-packages '(tide
                            typescript-mode
                            web-mode
                            company
                            prettier-js
                            add-node-modules-path))

(require 'web-mode)
(require 'typescript-mode)
(require 'company)
(require 'flycheck)
(require 'tide)
(require 'prettier-js)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  ;; 该路径使相对项目根目录的
  (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")
  ;; (add-hook 'before-save-hook 'tide-format-before-save)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0)
  (local-set-key (kbd "C-M-i") 'completion-at-point))

(setq prettier-js-args '("--single-quote"
                         "--trailing-comma" "all"
                         "--bracket-spacing" "false"))

(add-hook 'typescript-mode-hook
          (lambda ()
            (setup-tide-mode)
            (prettier-js-mode)
            (hs-minor-mode 1)
            (setq typescript-indent-level 2)))

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (add-node-modules-path)
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (prettier-js-mode)
              (setup-tide-mode)
              (flycheck-add-mode 'typescript-tslint 'web-mode)
              (setq web-mode-markup-indent-offset 2)
              (setq web-mode-code-indent-offset 2)
              (setq web-mode-css-indent-offset 2))))

(provide 'personal-ts)
