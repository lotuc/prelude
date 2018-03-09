(prelude-require-packages '(org-plus-contrib
                            htmlize
                            ob-http))
(require 'org)
(require 'ox-latex)
(require 'org-habit)

;; directory
(setq org-directory "~/Documents/Orgnote")
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

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
   (ruby . t)
   (org . t)
   (ditaa . t)
   (latex . t)
   (dot . t)
   (plantuml . t)
   (emacs-lisp . t)
   (ledger . t)
   (gnuplot . t)
   (screen . nil)
   (shell . t)
   (python . t)
   (js . t)
   (sql . nil)
   (sqlite . t)
   (http . t)))

;; mac 可以使用 brew install plantuml 查看 which plantuml 获取 jar 文件，软链接到目标路径
;; ln -s <源jar文件> ~/.emacs.d/personal/plantuml.jar
(setq org-plantuml-jar-path
      (expand-file-name "plantuml.jar" prelude-personal-dir))

;;;; org-capture
(defconst lotuc-org-timeline-dir (concat org-directory "/timeline"))
(defconst lotuc-org-personal-files-dir (concat org-directory "/lotuc/"))

;; default note file
(setq org-default-notes-file (concat org-directory "/capture/default.org"))

;; (setq org-agenda-inhibit-startup nil)
(defun lotuc-refresh-org-agenda-files ()
  "Rescan specified directories for agenda files"
  (interactive)
  (setq org-agenda-files
        (append
         (directory-files-recursively lotuc-org-timeline-dir ".*.org" )
         (directory-files-recursively lotuc-org-personal-files-dir ".*.org"))))
;; shit
;; `,(directory-files-recursively
;;   (concat lotuc-org-personal-files-dir "projects")
;;   ".*.org")
;; (car org-agenda-custom-commands)
;; (setq org-agenda-custom-commands
;;       `(("c" "Desk Work" agenda "" ;; (1) (2) (3) (4)
;;          ((org-agenda-files `(,@(directory-files-recursively
;;                               (concat lotuc-org-personal-files-dir "projects")
;;                               ".*.org"))) ;; (5)
;;           ) ;; (5) cont.
;;          ) ;; (6)
;;         ))

;;  `,(directory-files-recursively
;;            (concat lotuc-org-personal-files-dir "projects")
;;            ".*.org")
;; (setq org-agenda-custom-commands
;;       `(("c" "Projects" tags-todo ""
;;          ((org-agenda-files (,@(directory-files-recursively
;;                                 (concat lotuc-org-personal-files-dir "projects")
;;                                 ".*.org")))))))

(lotuc-refresh-org-agenda-files)

(defun lotuc-org-capture ()
  "Wrapper for org-capture, calculate some capture path first"
  (interactive)
  (setq org-capture-templates
        `(
          ("c"
           "Capture to timeline"
           entry
           (file+headline ,(concat lotuc-org-timeline-dir
                                   (format-time-string "/%Y/W%W.org"))
                          ,(format-time-string "%m-%d"))
           "* LATER %?\n:PROPERTIES:\n:DATETIME: %U\n:END:\n")
          ("n" "Note")
          ("nc"
           "Kill ring head"
           entry
           (file+headline ,(concat lotuc-org-timeline-dir
                                   (format-time-string "/%Y/W%W.org"))
                          ,(format-time-string "%m-%d"))
           "* LATER %?%c\n:PROPERTIES:\n:DATETIME: %U\n:END:\n"
           :tree-type week)
          ("na"
           "Annotation"
           entry
           (file+headline ,(concat lotuc-org-timeline-dir
                                   (format-time-string "/%Y/W%W.org"))
                          ,(format-time-string "%m-%d"))
           "* LATER %?%A\n:PROPERTIES:\n:DATETIME:%U\n:END:\n"
           :tree-type week)))
  (org-capture))
(global-set-key "\C-cc" 'lotuc-org-capture)

;;;; export
(setq org-latex-logfiles-extensions
      '("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi"
        "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"))

;;;; tex
;; Use XeLaTeX to export PDF in Org-mode
(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))

(provide 'personal-org)
