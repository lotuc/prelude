(prelude-require-packages '(
                            org-plus-contrib
                            ag
                            docker
                            yasnippet-snippets
                            magithub
                            pdf-tools
                            auto-complete
                            protobuf-mode
                            scribble-mode
                            youdao-dictionary
                            proof-general))

(require 'magit)
(require 'pdf-tools)
(require 'calibre-mode)
(require 'cl)

;; https://stackoverflow.com/questions/5019724/in-emacs-what-does-this-error-mean-warning-cl-package-required-at-runtime
(eval-when-compile (require 'cl))

(pdf-tools-install)

;; https://github.com/bbatsov/prelude/issues/998
(defun fix-c-a ()
  (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-a") nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist)))
(add-hook 'eshell-mode-hook 'fix-c-a)
(add-hook 'haskell-interactive-mode-hook 'fix-c-a)
(add-hook 'coq-mode #'smartparens-mode)

(global-set-key (kbd "\C-cq") 'youdao-dictionary-search-at-point+)
(defalias 'youdao/search #'youdao-dictionary-search)

;; https://magit.vc/manual/magit/Performance.html
(setq magit-refresh-status-buffer nil)

(setq line-number-display-limit large-file-warning-threshold)
(setq line-number-display-limit-width 200)

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; https://github.com/bbatsov/prelude/issues/918
(global-undo-tree-mode -1)

(projectile-register-project-type 'pipenv '("Pipfile")
                                  :compile "pipenv install"
                                  :test ""
                                  :run ""
                                  :test-suffix ".test")
(add-to-list 'projectile-project-root-files "package.json")
(add-to-list 'projectile-globally-ignored-directories "node_modules")
(add-to-list 'projectile-globally-ignored-directories ".venv")
(setq flycheck-check-syntax-automatically '(mode-enabled save))

;; https://www.emacswiki.org/emacs/ErcProxy
(setq socks-noproxy '("localhost"))
(require 'socks)
(setq erc-server-connect-function 'socks-open-network-stream)
(setq socks-server (list "ss" "127.0.0.1" 1080 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions

(defvar xsdvi-path
  (expand-file-name "~/.emacs.d/personal/java/xsdvi/xsdvi.jar"))
(defun lotuc/trans-xsd-to-svg (in-xsd-file out-svg-file)
  (interactive
   (list (read-file-name "XSD:")
         (expand-file-name (read-string "SVG Filename:")
                           (read-directory-name "Output Directory:"))))
  (let* ((out-dir
          (expand-file-name (make-temp-name "xsdvi") temporary-file-directory))
         (out-file-name
          (expand-file-name (format "%s.svg" (file-name-base in-xsd-file))
                            out-dir))
         (cmd (format "cd \"%s\" && java -jar \"%s\" \"%s\""
                      out-dir
                      (expand-file-name xsdvi-path)
                      (expand-file-name in-xsd-file))))
    (if (file-exists-p out-svg-file)
        (format "File already exists: %s" out-svg-file)
      (progn
        (make-directory out-dir)
        (if (and (equal (shell-command cmd "*xsdvi*") 0)
                 (file-exists-p out-file-name))
            (and (copy-file out-file-name out-svg-file) out-svg-file)
          (message (format "export xsd %s to svg %s"
                           in-xsd-file out-svg-file)))))))

(defun lotuc/kill-other-buffers ()
  "Kill *all* other buffers, including special buffers.
crux-kill-other-buffers 'Doesn't mess with special buffers', we kill all others"
  (interactive)
  (when (y-or-n-p
         "Are you sure you want to kill *all* buffers but the current one? ")
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))))

(defun lotuc/kill-matching-buffers (regexp)
  "Kill buffers matching REGEXP without asking for confirmation."
  (interactive "sKill buffers matching this regular expression: ")
  (cl-flet ((kill-buffer-ask (buffer) (kill-buffer buffer)))
    (kill-matching-buffers regexp)))

(defun lotuc/cleanup-buffers ()
  (interactive)
  (cl-flet ((kill-buffer-ask (buffer) (kill-buffer buffer)))
    (dolist (e '("*lsp"
                 "magit"
                 "*ag search"
                 "*Backtrace*"
                 "*Completions*"
                 "*Flycheck"
                 "*Shell Command"))
      (kill-matching-buffers e))))

(defun lotuc/open-in-external-app ()
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
           (w32-shell-execute
            "open" (replace-regexp-in-string "/" "\\" $fpath t t))) $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath)
           (let ((process-connection-type nil))
             (start-process "" nil "xdg-open" $fpath))) $file-list))))))

(provide 'lotuc-misc)
