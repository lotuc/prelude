(prelude-require-packages '(pangu-spacing))
;; WenQuanYi:14 + Monaco:12，中英文字体宽度2:1对齐
;; |中文中文中文中文|
;; |abcdefghijklmnop|

(require 'pangu-spacing)
(global-pangu-spacing-mode 1)
(defun enable-pangu-spacing-hook ()
  ;; 添加空白到文档，而不只是展示上的
  (set (make-local-variable 'pangu-spacing-real-insert-separtor) t))
(add-hook 'org-mode-hook 'enable-pangu-spacing-hook)

(defun lotuc-set-cn-font-size (size)
  "设置字体大小，仅在有 WenQuanYi Zen Hei Mono + Monaco 时有效"
  (interactive (list (read-number "size: ")))
  (when (not (equal system-type 'cygwin))
    ;; windows Cygwin 环境中这段代码似乎不能运行
    (when (not (null (x-list-fonts "Monaco")))
      ;; Monaco
      (set-face-attribute 'default nil :font (format "Monaco %d" size))
      ;; 中文使用WenQuanYi
      (when (not (null (x-list-fonts "WenQuanYi Zen Hei Mono")))
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font
           (frame-parameter nil 'font)
           charset
           (font-spec :family "WenQuanYi Zen Hei Mono" :size (+ size 2))))))))

(lotuc-set-cn-font-size 12)

(provide 'personal-font)
