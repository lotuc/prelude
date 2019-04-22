(require 'json)

(defun json-refmt-before-save ()
  (interactive)
  (when (eq major-mode 'json-mode)
    (let ((point (point)))
      (setq json-encoding-default-indentation "    ")
      (json-pretty-print-buffer)
      (goto-char point))))

(defun my-json-hook ()
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 4))

(add-hook 'json-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'json-refmt-before-save)))
(add-hook 'json-mode-hook 'my-json-hook)

(provide 'lotuc-json)
