(require 'eshell)
(require 'em-dirs)
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

;; open file in eshell
(defalias 'o 'find-file)
(defalias 'oo 'find-file-other-window)

;; (prelude-require-packages '(eshell-prompt-extras))
;; (require 'eshell-prompt-extras)

;; elshell-prompt-extras
;; (with-eval-after-load "esh-opt"
;;   (autoload 'epe-theme-dakrone "eshell-prompt-extras")
;;   (setq eshell-highlight-prompt nil
;;         eshell-prompt-function 'epe-theme-dakrone))

(defun lotuc-eshell-here ()
  "Go to eshell and set current directory to the buffer's directory"
  (interactive)
  (let ((dir (file-name-directory (or (buffer-file-name)
                                      default-directory))))
    (eshell)
    (eshell/pushd ".")
    (cd dir)
    (goto-char (point-max))
    (eshell-kill-input)
    (eshell-send-input)))

(global-set-key (kbd "C-x C-m") 'lotuc-eshell-here)

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
            (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))

(defun pcomplete/sudo ()
  (let ((prec (pcomplete-arg 'last -1)))
    (cond ((string= "sudo" prec)
           (while (pcomplete-here*
                   (funcall pcomplete-command-completion-function)
                   (pcomplete-arg 'last) t))))))

(provide 'personal-eshell)
