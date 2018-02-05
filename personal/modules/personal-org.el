(prelude-require-packages '(org-plus-contrib
                            htmlize))
(require 'org)

(setq org-confirm-babel-evaluate nil)
(setq org-startup-indented t)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-export-with-smart-quotes t)
'(org-cycle-include-plain-lists t)
'(org-hide-leading-stars t)
'(org-alphabetical-lists t)
'(org-koma-letter-prefer-subject t)
'(ebib-bibtex-dialect (quote biblatex))


(org-add-link-type
 "color" nil
 (lambda (path desc format)p
   (cond
    ((eq format 'html)
     (format "<span style=\"color:%s;\">%s</span>" path desc))
    ((eq format 'latex)
     (format "{\\color{%s}%s}" path desc)))))

(org-add-link-type
 "hl" nil
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "<font style=\"background-color:%s;\">%s</font>" path desc))
    ((eq format 'latex)
     (format "\\colorbox{%s}{%s}" path desc))))) ;; require \usepackage{color}

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (R . t)
   (org . t)
   (ditaa . t)
   (latex . t)
   (dot . t)
   (emacs-lisp . t)
   (ledger . t)
   (gnuplot . t)
   (screen . nil)
   (shell . t)
   (sql . nil)
   (sqlite . t)))

(provide 'personal-org)
