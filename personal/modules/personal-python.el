(prelude-require-packages '(pipenv python-docstring))
(require 'eshell)
(require 'pipenv)

;; pythonic-activate RET /path/to/virtualenv 可用于指定虚拟环境

(defun personal-python-mode-hook ()
  ;; C-c M-d
  (require 'python-docstring)
  (python-docstring-mode 1)
  (pipenv-mode)
  (anaconda-eldoc-mode)
  (setq-local pipenv-projectile-after-switch-function
              #'pipenv-projectile-after-switch-extended))

(add-hook 'python-mode-hook 'personal-python-mode-hook)
(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "pr" "pipenv run $*")
            (eshell/alias "prp" "pipenv run python $*")))

(provide 'personal-python)
