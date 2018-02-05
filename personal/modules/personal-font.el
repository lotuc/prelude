;; WenQuanYi:14 + Monaco:12，中英文字体宽度2:1对齐
(set-face-attribute 'default nil :font "Monaco 12")
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font)
   charset
   (font-spec :family "WenQuanYi Zen Hei Mono" :size 14)))

(provide 'personal-font)
