(prelude-require-packages '(sql-indent))

(require 'sql-indent)

(add-hook 'sql-mode-hook (lambda () (sqlind-minor-mode)))

(provide 'lotuc-sql)
