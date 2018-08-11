(require 'package)
(setq package-archives '(
                         ("gnu"   . "https://elpa.emacs-china.org/gnu/")
                         ("melpa" . "https://elpa.emacs-china.org/melpa/")
                         ;; ("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ;; ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ;; ("melpa" . "https://melpa.org/packages/")
                         ;; ("org" . "https://orgmode.org/elpa/")
                         ("org-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;; https://blog.vifortech.com/posts/emacs-tls-fix/
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(setq prelude-theme
      ;; 'flatui
      ;; 'zenburn
      ;; 'dracula
      ;; 'solarized-dark
      nil
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
(defun disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'disable-scroll-bars)


;; jump when cursor moves out of screen
(setq scroll-conservatively 101)
(setq scroll-margin 2)

(setq inhibit-startup-screen t
      initial-scratch-message "" make-backup-files nil)

;; https://github.com/bbatsov/solarized-emacs/issues/143
(setq solarized-use-variable-pitch nil)
(setq solarized-scale-org-headlines nil)

(defun open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2016-10-15"
  (interactive)
  (let* (
         ($file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" $fpath t t))) $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath))) $file-list))))))

(server-start)
