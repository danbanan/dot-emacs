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
(define-key key-translation-map (kbd "s-z") (kbd "«"))
(define-key key-translation-map (kbd "s-x") (kbd "»"))

;;; Backspace and enter replacements
;; (define-key global-map (kbd "<backspace>") 'ignore)
(define-key global-map (kbd "C-?") 'help-command)
(define-key global-map (kbd "M-?") 'mark-paragraph)
(define-key global-map (kbd "C-h") 'delete-backward-char)
(define-key global-map (kbd "M-h") 'backward-kill-word)
(define-key global-map (kbd "C-x C-b") 'previous-buffer)

;;; Help mode
(define-key help-mode-map (kbd "<return>") 'help-follow)

;;; Org-mode
(define-key global-map (kbd "C-c l") #'org-store-link)
(define-key global-map (kbd "C-c a") #'org-agenda)
(define-key global-map (kbd "C-c c") #'org-capture)
;; (define-key org-mode-map (kbd "C-c ]") #'org-ref-insert-ref-link)
;; (define-key org-mode-map (kbd "C-c SPC") 'counsel-outline)


;;; Markdown-mode
(define-key markdown-mode-map (kbd "C-c SPC") 'counsel-outline)


;;; Elfeed
(define-key global-map (kbd "C-x W") 'elfeed)


;;; Ebuku
(define-key global-map (kbd "C-x B") 'ebuku-add-bookmark)

;;; Rust
(define-key rust-mode-map (kbd "C-c C-c") 'rust-check)
(define-key rust-mode-map (kbd "C-c C-r") 'rust-run)
(define-key rust-mode-map (kbd "C-c C-b") 'rust-compile)
(define-key rust-mode-map (kbd "C-c C-t") 'rust-test)
(define-key rust-mode-map (kbd "TAB") 'company-indent-or-complete-common)


;;; Company
(define-key company-mode-map (kbd "C-M-i") 'company-complete)
(define-key company-active-map (kbd "C-h") 'delete-backward-char)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

;;; Elisp
(define-key emacs-lisp-mode-map (kbd "<C-tab>") 'counsel-company)
(define-key emacs-lisp-mode-map (kbd "C-c SPC") 'counsel-outline)


;; C and C++
(define-key c-mode-map (kbd "<C-tab>") 'counsel-company)
;; (define-key c-mode-map (kbd "<C-M-tab>") 'clang-format-buffer)
(define-key c-mode-map (kbd "C-c C-c") 'compile)
(define-key c++-mode-map (kbd "C-c C-c") 'compile)


;;; Frame
(define-key global-map (kbd "C-x C-c") 'ask-before-closing)
(define-key global-map (kbd "C-z") 'ignore)
(define-key global-map (kbd "C-x C-z") 'ignore)


;;; Dired
(define-key dired-mode-map (kbd "C-h") 'dired-unmark-backward)

;;; Vterm
(define-key vterm-mode-map (kbd "C-h") 'vterm-send-backspace)
(define-key global-map (kbd "C-x V") 'db/vterm-toggle-cd)

;;; Ace-window
(define-key global-map (kbd "C-x o") #'ace-window)


;;; Multiple cursors
(define-key global-map (kbd "C-S-c C-S-c") 'mc/edit-lines)
(define-key global-map (kbd "C->") 'mc/mark-next-like-this)
(define-key global-map (kbd "C-<") 'mc/mark-previous-like-this)
(define-key global-map (kbd "C-c C-<") 'mc/mark-all-like-this)


;;; Terminal Here
(define-key global-map (kbd "C-x T") #'terminal-here-launch)


;;; Scheme
(require 'scheme)
(define-key key-translation-map (kbd "C-x 8 +") (kbd "＋"))
(define-key key-translation-map (kbd "C-x 8 -" ) (kbd "－"))
(define-key key-translation-map (kbd "C-x 8 /") (kbd "÷"))
(define-key key-translation-map (kbd "C-x 8 <up>") (kbd "↑"))


;; LSP mode
(define-key lsp-mode-map (kbd "M-.") #'lsp-find-implementation)

;;; Perl
(require 'cperl-mode)			; may not be necessary
(define-key cperl-mode-map (kbd "C-c C-c c") #'compile)
(define-key cperl-mode-map (kbd "C-c C-c r") #'recompile)
(define-key cperl-mode-map (kbd "C-c C-c d") #'cperl-db)

;;; GDB
;; (define-key gud-minor-mode-map (kbd "C-c C-w") nil)
;; (define-key gud-minor-mode-map (kbd "C-c w") #')

;; ESS
;; (define-key ess-mode-map (kbd "C-c h") #'ess-help)
