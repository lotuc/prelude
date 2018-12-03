(defconst personal-modules (expand-file-name "modules" prelude-personal-dir))
(defconst personal-packages (expand-file-name "packages" prelude-personal-dir))
(defconst private-settings (expand-file-name "private" prelude-personal-dir))
(add-to-list 'load-path personal-modules)
(add-to-list 'load-path personal-packages)

(require 'lotuc-ui)
(require 'lotuc-org)
(require 'lotuc-font)
(require 'lotuc-misc)
(require 'lotuc-eshell)
(require 'lotuc-lilypond)

;; (require 'lotuc-lsp)
(require 'lotuc-lsp-js-ts)
(require 'lotuc-lsp-python)

(require 'lotuc-go)
(require 'lotuc-sql)
(require 'lotuc-xml)
(require 'lotuc-rust)
(require 'lotuc-reason)
(require 'lotuc-haskell)
(require 'lotuc-markdown)
(require 'lotuc-purescript)


;; Load private settings
(let* ((private-setting (expand-file-name "init.el" private-settings)))
  (when (file-exists-p private-setting)
    (load-file private-setting)))
