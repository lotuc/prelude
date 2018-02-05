(when (eq system-type 'darwin)
  (progn
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)
    (message "Command is now bound to META and Option is bound to SUPER.")))

(provide 'personal-osx)
