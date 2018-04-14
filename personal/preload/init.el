(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(setq prelude-theme
      'flatui
      ;; 'zenburn
      ;; 'dracula
      ;; 'solarized-dark
      ;; nil
      )

(line-number-mode 1)
(setq-default indicate-empty-lines t)

;; transparent
;; (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;; Smooth Scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(toggle-scroll-bar -1)

;; jump when cursor moves out of screen
(setq scroll-conservatively 101)
(setq scroll-margin 2)

(setq inhibit-startup-screen t
      initial-scratch-message "" make-backup-files nil)

(server-start)
