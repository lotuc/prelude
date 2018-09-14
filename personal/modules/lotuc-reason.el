(require 'prelude-programming)

(prelude-require-packages '(reason-mode merlin))

(require 'reason-mode)
(require 'merlin-ac)

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))

(defun setup-refmt ()
  (defun shell-cmd (cmd)
    "Returns the stdout output of a shell command or nil if the command returned
   an error"
    (car (ignore-errors (apply 'process-lines (split-string cmd)))))

  (let* ((refmt-bin (shell-cmd "which refmt")))
    (when refmt-bin
      (setq refmt-command refmt-bin))))

(add-hook 'reason-mode-hook
          (lambda ()
            (setup-refmt)
            (add-hook 'before-save-hook 'refmt-before-save)
            (merlin-mode)))

(setq merlin-ac-setup t)

(provide 'lotuc-reason)
