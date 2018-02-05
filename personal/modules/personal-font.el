;; WenQuanYi:14 + Monaco:12，中英文字体宽度2:1对齐

(when (not (equal system-type 'cygwin))
  ;; windows Cygwin 环境中这段代码似乎不能运行
  (when (not (null (x-list-fonts "Monaco")))
    ;; Monaco
    (set-face-attribute 'default nil :font "Monaco 12")
    ;; 中文使用WenQuanYi
    (when (not (null (x-list-fonts "WenQuanYi Zen Hei Mono")))
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font
         (frame-parameter nil 'font)
         charset
         (font-spec :family "WenQuanYi Zen Hei Mono" :size 14))))))

(provide 'personal-font)
