(prelude-require-packages '(psc-ide))

;; https://github.com/epost/psc-ide-emacs
(require 'psc-ide)

(add-hook 'purescript-mode-hook
          (lambda ()
            (psc-ide-mode)
            (company-mode)
            (flycheck-mode)
            (turn-on-purescript-indentation)))

(provide 'lotuc-purescript)
