(require 'company)

(defun lotuc-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq-default)
  (setq tab-width 2)
  (setq standard-indent 2)
  (setq indent-tabs-mode nil)
  (company-mode)
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command)))
(add-hook 'go-mode-hook 'lotuc-go-mode-hook)

(provide 'lotuc-go)
