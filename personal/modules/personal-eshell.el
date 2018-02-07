(prelude-require-packages '(eshell-prompt-extras))
(require 'eshell-prompt-extras)

;; 方便清除 eshell buffer
(defun eshell/c ()
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))

(add-hook 'eshell-mode-hook
          (lambda ()
            ;; 不知道为什么要显式的执行这个
            ;; 不执行的话，虽然环境变量正确但是 which 并不能找到正确命令
            (eshell/addpath "/usr/local/bin")
            ;; 启动时那条消息清一下
            (eshell/c)))

;; elshell-prompt-extras
(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-dakrone "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-dakrone))

(provide 'personal-eshell)
