(prelude-require-packages '(lsp-mode
                            lsp-python
                            pipenv
                            pyvenv
                            python-docstring))

(require 'prelude-programming)
(require 'lsp-mode)
(require 'lsp-python)
(require 'eshell)
(require 'pipenv)
(require 'cl)

;; https://github.com/davidhalter/jedi-vim/issues/704
;; https://github.com/davidhalter/jedi/pull/829/commits
;; https://github.com/davidhalter/jedi-vim/issues/685

;; https://github.com/palantir/python-language-server
;; pip install 'python-language-server[all]'
(defun find-python-project-root ()
  (interactive)
  (let* ((name #'(lambda (dir)
                   (directory-files
                    dir
                    nil
                    "setup.py\\|Pipfile\\|setup.cfg\\|.venv\\|tox.ini\\|.git")))
         (dir (locate-dominating-file "." name)))
    (if dir (file-truename dir)
      (progn
        (message "Couldn't find project root, using the current directory as the root.")
        default-directory))))

;; projectile
(lsp-define-stdio-client
 lsp-python "python" #'find-python-project-root '("pyls"))

(defun find-venv-path (path)
  "Find .venv directory from path up to root"
  (if (file-exists-p (concat path "bin/python"))
      path
    (let ((f1 (locate-dominating-file path ".venv")))
      (message f1)
      (if (and f1 (file-exists-p (concat f1 ".venv/bin/python")))
          (concat f1 ".venv")))))

(defun lotuc/set-virtualenv-dir (path)
  "Prompt user to enter the virtualenv directory."
  (interactive
   (list (read-file-name "Set virtualenv directory:")))
  (let ((path (find-venv-path (file-name-as-directory path))))
    (if path
        (progn (let ((venv-pylint (concat path "bin/pylint")))
                 (if (file-exists-p venv-pylint)
                     (flycheck-set-checker-executable
                      'python-pylint venv-pylint)))
               (pyvenv-activate path)
               (message path)
               (lsp-restart-workspace)
               (flet ((kill-buffer-ask (buffer) (kill-buffer buffer)))
                 (dolist (e '("*lsp-python stderr"))
                   (kill-matching-buffers e))))
      (message "venv not found"))))

(defun lotuc/unset-virtualenv-dir ()
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
  (subword-mode +1)
  (eldoc-mode 1)
  (python-docstring-mode)
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

(add-hook 'python-mode-hook 'prelude-python-mode-defaults)

(provide 'lotuc-lsp-python)
