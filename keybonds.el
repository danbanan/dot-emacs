;;; Modifier keys
(cond ((string-equal system-type "darwin")
       (setq mac-option-modifier 'meta)
       (setq mac-command-modifier 'super))
      ((string-equal system-type "windows-nt")
       (setq w32-pass-alt-to-system nil)
       (setq w32-rwindow-modifier 'super)
       (setq w32-lwindow-modifier 'super)
       (w32-register-hot-key [M-tab])
       (w32-register-hot-key [s-])))


;;; Norwegian letters
(define-key key-translation-map (kbd "s-'") (kbd "æ"))
(define-key key-translation-map (kbd "s-o") (kbd "ø"))
(define-key key-translation-map (kbd "s-a") (kbd "å"))
(define-key key-translation-map (kbd "s-\"") (kbd "Æ"))
(define-key key-translation-map (kbd "s-O") (kbd "Ø"))
(define-key key-translation-map (kbd "s-A") (kbd "Å"))


;;; Backspace and enter replacements
(define-key global-map (kbd "<return>") 'ignore)
(define-key global-map (kbd "<backspace>") 'ignore)
(define-key global-map (kbd "C-j") 'newline)
(define-key global-map (kbd "C-?") 'help-command)
(define-key global-map (kbd "M-?") 'mark-paragraph)
(define-key global-map (kbd "C-h") 'delete-backward-char)
(define-key global-map (kbd "M-h") 'backward-kill-word)


;;; Help mode
(define-key help-mode-map (kbd "<return>") 'help-follow)


;;; Counsel
(define-key global-map (kbd "C-x j f") 'counsel-file-jump)
(define-key global-map (kbd "C-x j d") 'counsel-dired-jump)


;;; Org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key org-mode-map (kbd "C-c SPC") 'counsel-outline)


;;; Markdown-mode
(define-key markdown-mode-map (kbd "C-c SPC") 'counsel-outline)


;; Elfeed
(define-key global-map (kbd "C-x W") 'elfeed)


;; PDF-tools
(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)


;;; Rust
(define-key rust-mode-map (kbd "C-c C-c") 'rust-check)
(define-key rust-mode-map (kbd "C-c C-r") 'rust-run)
(define-key rust-mode-map (kbd "C-c C-b") 'rust-compile)
(define-key rust-mode-map (kbd "C-c C-t") 'rust-test)
(define-key rust-mode-map (kbd "TAB") 'company-indent-or-complete-common)


;;; Elisp
(define-key emacs-lisp-mode-map (kbd "<C-tab>") 'counsel-company)
(define-key emacs-lisp-mode-map (kbd "C-c SPC") 'counsel-outline)


;; C and C++
(define-key c-mode-map (kbd "<C-tab>") 'counsel-company)
;; (define-key c-mode-map (kbd "<C-M-tab>") 'clang-format-buffer)


;;; Frame
(define-key global-map (kbd "C-x C-c") 'ask-before-closing)
(define-key global-map (kbd "C-z") 'ignore)
(define-key global-map (kbd "C-x C-z") 'ignore)


;;; Dired
(define-key dired-mode-map (kbd "C-j") 'dired-find-file)
(define-key dired-mode-map (kbd "C-h") 'dired-unmark-backward)


;;; Comint
(define-key comint-mode-map (kbd "C-j") 'comint-send-input)


;;; Eshell

