(require 'company)

(defun lotuc-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq-default)
  (setq tab-width 2)
  (setq standard-indent 2)
  (setq indent-tabs-mode nil)
  (company-mode)
  (setq company-tooltip-limit 20)                      ; bigger popup window
  (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                          ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  )
(add-hook 'go-mode-hook 'lotuc-go-mode-hook)

(provide 'personal-go)
