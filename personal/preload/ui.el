(setq prelude-theme
      'zenburn
      ;; 'dracula
      ;; 'solarized-dark
      ;; nil
      )

;; show line number
(global-linum-mode 1)
;; https://coldnew.github.io/f0802775/
(setq inhibit-linum-mode-alist
      '(w3m-mode
        eshell-mode
        shell-mode
        term-mode
        nxml-mode
        elfeed-search-mode))
(defadvice linum-on (around inhibit-for-modes activate)
  "Stop turing linum-mode if it is in the inhibit-linum-mode-alist."
  (unless (member major-mode inhibit-linum-mode-alist)
    ad-do-it))

;; highlight selected text
(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")

;; transparent
(set-frame-parameter (selected-frame) 'alpha '(90 . 100))
(add-to-list 'default-frame-alist '(alpha . (95 . 90)))

;; Smooth Scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(toggle-scroll-bar -1)

;; jump when cursor moves out of screen
(setq scroll-conservatively 101)
(setq scroll-margin 2)
(setq-default indicate-empty-lines t)

(setq inhibit-startup-screen t
      initial-scratch-message ""
      make-backup-files nil)
;;
(add-hook 'org-mode-hook
          (progn
           (lambda ()
             (dolist (face '(org-level-1
                             org-level-2
                             org-level-3
                             org-level-4
                             org-level-5))
               (set-face-attribute face nil :weight 'semi-bold :height 1.0))
             (setq solarized-scale-org-headlines nil))))
