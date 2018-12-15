(require 'package)

(setq package-archives '(
                          ;; ("gnu"   . "https://elpa.emacs-china.org/gnu/")
                         ;; ("melpa" . "https://elpa.emacs-china.org/melpa/")
                          ("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                          ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                          ;; ("melpa" . "https://melpa.org/packages/")
                         ;; ("org" . "https://orgmode.org/elpa/")
                         ("org-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(line-number-mode 1)
(toggle-scroll-bar -1)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq-default indicate-empty-lines t)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq inhibit-startup-screen t initial-scratch-message "" make-backup-files nil)

;; Smooth Scrolling
(setq scroll-step 1)
(setq scroll-margin 2)
(setq scroll-conservatively 101)
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(defun disable-scroll-bars (frame)
  (modify-frame-parameters
   frame '((vertical-scroll-bars . nil)
           (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'disable-scroll-bars)

(setq prelude-theme
      ;; 'flatui
      ;; 'zenburn
      ;; 'dracula
      'solarized-dark)
;; Solarized theme related
;; https://github.com/bbatsov/solarized-emacs/issues/143
(setq solarized-use-variable-pitch nil)
(setq solarized-scale-org-headlines nil)

;; https://blog.vifortech.com/posts/emacs-tls-fix/
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

(server-start)
