(add-to-list 'load-path (expand-file-name "modules" prelude-personal-dir))

(require 'personal-private)
(require 'personal-eshell)
(require 'personal-font)
(require 'personal-osx)
(require 'personal-ui)
(require 'personal-misc)
(require 'personal-xml)
(require 'personal-rust)
(require 'personal-org)
(require 'personal-ts)
(require 'calibre-mode)

(setq calibre-root-dir (expand-file-name "~/Calibre Library/Library"))
(setq calibre-db (concat calibre-root-dir "/metadata.db"))