(defconst personal-modules (expand-file-name "modules" prelude-personal-dir))
(prelude-require-packages '(scribble-mode gruvbox-theme))
(add-to-list 'load-path personal-modules)
(add-to-list 'load-path (expand-file-name "lilypond" personal-modules))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(require 'personal-eshell)
(require 'personal-font)
(require 'personal-osx)
(require 'personal-ui)
(require 'personal-misc)
(require 'personal-xml)
(require 'personal-rust)
(require 'personal-org)
(require 'personal-ts)
(require 'personal-blog)
(require 'personal-python)
(require 'personal-lilypond)
(require 'personal-go)
(require 'personal-json)
(require 'personal-haskell)
(require 'personal-reason)
(require 'personal-purescript)
(require 'personal-sql)
(require 'personal-lsp)
(require 'personal-java)

(require 'calibre-mode)

(setq org-default-notes-file "~/Workspace/org/refile.org")
(setq calibre-root-dir (expand-file-name "~/Calibre Library/Library"))
(setq calibre-db (concat calibre-root-dir "/metadata.db"))
(setq bbdb-file "~/.emacs.d/personal/private/bbdb")

;; Load private settings
(let* ((private-dir (expand-file-name "private" prelude-personal-dir))
       (private-setting (expand-file-name "init.el" private-dir)))
  (when (file-exists-p private-setting)
    (load-file private-setting)))

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
