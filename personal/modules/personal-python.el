(prelude-require-packages '(lsp-mode
                            lsp-python
                            pipenv
                            pyvenv))

(require 'prelude-programming)
(require 'lsp-mode)
(require 'lsp-python)
(require 'eshell)
(require 'pipenv)

;; https://github.com/palantir/python-language-server
;; pip install 'python-language-server[all]'

(defun find-python-project-root ()
  (interactive)
  (condition-case ex
      (projectile-project-root)
    ('error (let* ((name "setup.py\\|Pipfile\\|setup.cfg\\|tox.ini")
                   (dir (locate-dominating-file "." name)))
              (if dir (file-truename dir)
                (progn
	            (message "Couldn't find project root, using the current directory as the root.")
                    default-directory))))))

;; projectile
(lsp-define-stdio-client lsp-python "python"
                         #'find-python-project-root
                         '("pyls"))

;; pipenv
(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "pr" "pipenv run $*")
            (eshell/alias "prp" "pipenv run python $*")))

(defun find-venv-path (path)
  (if (file-exists-p (concat path "bin/python"))
      path
    (let ((f1 (locate-dominating-file path ".venv")))
      (message f1)
      (if (and f1 (file-exists-p (concat f1 ".venv/bin/python")))
          (concat f1 ".venv")))))

;; pythonic-activate RET / path/to/virtualenv 可用于指定虚拟环境
(defun set-virtualenv-dir (path)
  "Prompt user to enter the virtualenv directory."
  (interactive
   (list
    (read-file-name "Set virtualenv directory:")))
  (let ((path (find-venv-path (file-name-as-directory path))))
    (if path
        (progn (let ((venv-pylint (concat path "bin/pylint")))
                 (if (file-exists-p venv-pylint)
                     (flycheck-set-checker-executable
                      'python-pylint venv-pylint)))
               (pyvenv-activate path)
               (message path)
               (lsp-restart-workspace))
      (message "venv not found"))))

(defun unset-virtualenv-dir ()
  (interactive)
  (pyvenv-deactivate))

;; ============== FROM prelude-python ===============
(defun prelude-python--encoding-comment-required-p ()
  (re-search-forward "[^\0-\177]" nil t))

(defun prelude-python--detect-encoding ()
  (let ((coding-system
         (or save-buffer-coding-system
             buffer-file-coding-system)))
    (if coding-system
        (symbol-name
         (or (coding-system-get coding-system 'mime-charset)
             (coding-system-change-eol-conversion coding-system nil)))
      "ascii-8bit")))

(defun prelude-python--insert-coding-comment (encoding)
  (let ((newlines (if (looking-at "^\\s *$") "\n" "\n\n")))
    (insert (format "# coding: %s" encoding) newlines)))

(defun prelude-python-mode-set-encoding ()
  "Insert a magic comment header with the proper encoding if necessary."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (prelude-python--encoding-comment-required-p)
      (goto-char (point-min))
      (let ((coding-system (prelude-python--detect-encoding)))
        (when coding-system
          (if (looking-at "^#!") (beginning-of-line 2))
          (cond ((looking-at "\\s *#\\s *.*\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)")
                 ;; update existing encoding comment if necessary
                 (unless (string= (match-string 2) coding-system)
                   (goto-char (match-beginning 2))
                   (delete-region (point) (match-end 2))
                   (insert coding-system)))
                ((looking-at "\\s *#.*coding\\s *[:=]"))
                (t (prelude-python--insert-coding-comment coding-system)))
          (when (buffer-modified-p)
            (basic-save-buffer-1)))))))

(defun personal-python-save-hook ()
  (interactive)
  (prelude-python-mode-set-encoding)
  (lsp-format-buffer))

(when (fboundp 'exec-path-from-shell-copy-env)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(defun prelude-python-mode-defaults ()
  "Defaults for Python programming."

  (if (not pyvenv-virtual-env)
      (set-virtualenv-dir "."))
  (if pyvenv-virtual-env
      (setenv "VIRTUAL_ENV" pyvenv-virtual-env))

  (subword-mode +1)
  (eldoc-mode 1)
  (setq-local electric-layout-rules
              '((?: . (lambda ()
                        (and (zerop (first (syntax-ppss)))
                             (python-info-statement-starts-block-p)
                             'after)))))
  (when (fboundp #'python-imenu-create-flat-index)
    (setq-local imenu-create-index-function
                #'python-imenu-create-flat-index))
  (add-hook 'post-self-insert-hook
            #'electric-layout-post-self-insert-function nil 'local)
  (add-hook 'after-save-hook 'personal-python-save-hook nil 'local)
  (pipenv-mode)
  (lsp-python-enable)
  (setq python-shell-interpreter "ipython")
  (setq-local pipenv-projectile-after-switch-function
              #'pipenv-projectile-after-switch-extended))

(setq prelude-python-mode-hook 'prelude-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'prelude-python-mode-hook)))

(provide 'personal-python)
