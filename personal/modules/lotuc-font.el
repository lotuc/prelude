(prelude-require-packages '(pangu-spacing))
;; 以下字体可以让英文宽度：中文宽度为 1:2
;; Monaco:12 : WenQuanYi Zen Hei Mono:14
;; Monaco:12 : Kaiti SC:14

;; |中文中文中文中文|中文中文中文中文中文中文中文中文中文中文中文中文|
;; |abcdefghijklmnop|abcdefghijklmnopabcdefghijklmnopabcdefghijklmnop|

(require 'pangu-spacing)

(defun lotuc/pangu-enable-spacing ()
  "添加空白到文档，而不只是展示上的"
  (interactive)
  (set (make-local-variable 'pangu-spacing-real-insert-separtor) t))

(defun lotuc/set-cn-kaiti (size &optional kaitisize)
  (interactive (list (read-number "size: " 12)
                     (read-number "kaitisize:" 0)))
  (when (not (equal system-type 'cygwin))
    ;; windows Cygwin 环境中这段代码似乎不能运行
    (when (not (null (x-list-fonts "Monaco")))
      ;; Monaco: 12
      (set-face-attribute 'default nil :font (format "Monaco %d" size))
      ;; 中文使用Kaiti SC:13
      (when (not (null (x-list-fonts "Kaiti SC")))
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font
           (frame-parameter nil 'font)
           charset
           (font-spec :family "Kaiti SC"
                      :size (if (or (not kaitisize) (= kaitisize 0))
                                (+ size 2)
                              kaitisize))))))))
;; 15 - 18
;; (lotuc/set-cn-kaiti 12)

(defun lotuc/set-cn-font-size (size)
  "设置字体大小，仅在有 WenQuanYi Zen Hei Mono + Monaco 时有效"
  (interactive (list (read-number "size: ")))
  (when (not (equal system-type 'cygwin))
    ;; windows Cygwin 环境中这段代码似乎不能运行
    (when (not (null (x-list-fonts "Monaco")))
      ;; Monaco
      (set-face-attribute 'default nil :font (format "Monaco %d" size))
      ;; 中文使用 WenQuanYi
      (when (not (null (x-list-fonts "WenQuanYi Zen Hei Mono")))
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font
           (frame-parameter nil 'font)
           charset
           (font-spec :family "WenQuanYi Zen Hei Mono" :size (+ size 2))))))))

(defun lotuc/default-font ()
  (interactive)
    (when (not (null (x-list-fonts "Monaco")))
      (set-face-attribute 'default nil :font (format "Monaco %d" 13))
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font
         (frame-parameter nil 'font)
         charset
         (font-spec :family "Monaco")))))

(lotuc/default-font)

(provide 'lotuc-font)
