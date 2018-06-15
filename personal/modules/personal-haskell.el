(prelude-require-packages '(intero
                            ;; shm
                            ))

(eval-after-load 'haskell-mode
  '(progn
     (defun prelude-haskell-mode-defaults ()
       ;; (require 'shm)
       (subword-mode +1)
       (intero-mode +1)
       ;; (structured-haskell-mode 1)
       )

     (setq prelude-haskell-mode-hook 'prelude-haskell-mode-defaults)

     (add-hook 'haskell-mode-hook (lambda ()
                                    (run-hooks 'prelude-haskell-mode-hook)))))

(provide 'personal-haskell)
