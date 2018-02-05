;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
;; (add-hook 'rust-mode-hook  #'company-mode)
;; (add-hook 'rust-mode-hook  #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'rust-mode-hook
;;           '(lambda ()
;;              (cargo-minor-mode)
;;              (flycheck-rust-setup)
;;              (smartparens-mode -1)
;;           (setq racer-cmd (concat (getenv "HOME") "/.cargo/bin/racer"))
;;           (setq racer-rust-src-path
;;                    (concat
;;                     (getenv "HOME")
;;                     "/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"))
;;              (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
;;          (electric-pair-mode 1)))
(eval-after-load 'rust-mode
  '(progn
     (local-set-key (kbd "C-c c") 'rust-compile)))

(provide 'personal-rust)
