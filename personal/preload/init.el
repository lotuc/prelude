(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(server-start)
