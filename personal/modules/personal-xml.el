(prelude-require-packages '(hideshow))
(require 'nxml-mode)
(require 'sgml-mode)

(add-hook 'nxml-mode-hook 'hs-minor-mode)
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"
               "<!--"
               sgml-skip-tag-forward
               nil))
(add-hook 'nxml-mode-hook
          (lambda ()
            (pangu-spacing-mode -1)
            (flycheck-mode -1)
            (projectile-mode -1)
            (whitespace-mode -1)
            ;; (font-lock-mode -1)
            (lotuc-xml-read-only)))

;;;###autoload
(defun lotuc-xml-read-only ()
  "readonly on nxml-mode"
  (interactive)
  (when (derived-mode-p 'nxml-mode)
    (setq buffer-read-only -1)
    (jit-lock-mode -1)
    (buffer-disable-undo)))

;;;###autoload
(defun lotuc-xml-edit ()
  "turn on editing on nxml-mode"
  (interactive)
  (when (derived-mode-p 'nxml-mode)
    (setq buffer-read-only nil)
    (jit-lock-mode t)
    (buffer-enable-undo)))

(defun lotuc-xml-format-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (when (derived-mode-p 'nxml-mode)
    (setq buffer-read-only nil)
    (jit-lock-mode t)
    (buffer-enable-undo)
    (save-excursion
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))))

(provide 'personal-xml)
