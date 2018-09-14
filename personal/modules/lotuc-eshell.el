(require 'eshell)
(require 'em-dirs)

;; Clear eshell
(defun eshell/c ()
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))

(add-hook 'eshell-mode-hook
          (lambda ()
            )
          )

(defalias 'o 'find-file)
(defalias 'oo 'find-file-other-window)

(defun lotuc/eshell-hook ()
  ;; don't know why
  ;; without this, 'which' cannot find actual command
  (eshell/addpath "/usr/local/bin")
  (eshell-cmpl-initialize)
  (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
  (eshell/c))
(add-hook 'eshell-mode-hook 'lotuc/eshell-hook)

(defun pcomplete/sudo ()
  (let ((prec (pcomplete-arg 'last -1)))
    (cond ((string= "sudo" prec)
           (while (pcomplete-here*
                   (funcall pcomplete-command-completion-function)
                   (pcomplete-arg 'last) t))))))

(defun lotuc/eshell-here ()
  "Go to eshell and set current directory to the buffer's directory"
  (interactive)
  (let ((dir (file-name-directory
              (or (buffer-file-name) default-directory))))
    (eshell)
    (eshell/pushd ".")
    (cd dir)
    (goto-char (point-max))
    (eshell-kill-input)
    (eshell-send-input)))

(global-set-key (kbd "C-x M-m") 'lotuc/eshell-here)

(provide 'lotuc-eshell)
