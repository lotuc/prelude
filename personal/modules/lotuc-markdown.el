(prelude-require-packages '(markdown-toc
                            markdownfmt))

(require 'whitespace)

(markdownfmt-enable-on-save)
(defun turn-off-whitespace-hook ()
  (whitespace-mode -1))
(add-hook 'gfm-mode-hook 'turn-off-whitespace-hook)

(provide 'lotuc-markdown)
