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
;; (define-key global-map (kbd "<backspace>") 'ignore)
(define-key global-map (kbd "C-j") 'newline)
(define-key global-map (kbd "C-?") 'help-command)
(define-key global-map (kbd "M-?") 'mark-paragraph)
(define-key global-map (kbd "C-h") 'delete-backward-char)
(define-key global-map (kbd "M-h") 'backward-kill-word)


;;; Help mode
(define-key help-mode-map (kbd "<return>") 'help-follow)


;;; Ivy
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-alt-done)


;;; Counsel
(define-key global-map (kbd "C-x b") 'counsel-switch-buffer)
(define-key global-map (kbd "C-x 4 b") 'counsel-switch-buffer-other-window)
(define-key global-map (kbd "C-x j f") 'counsel-file-jump)
(define-key global-map (kbd "C-x j d") 'counsel-dired-jump)


;;; Swiper
(define-key global-map (kbd "C-s") 'swiper-isearch)
(define-key global-map (kbd "C-r") 'swiper-isearch-backward)


;;; Org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key org-mode-map (kbd "C-c SPC") 'counsel-outline)


;;; Markdown-mode
(define-key markdown-mode-map (kbd "C-c SPC") 'counsel-outline)


;;; Elfeed
(define-key global-map (kbd "C-x W") 'elfeed)


;;; Ebuku
(define-key global-map (kbd "C-x B") 'ebuku-add-bookmark)


;; PDF-tools
(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)


;;; Rust
(define-key rust-mode-map (kbd "C-c C-c") 'rust-check)
(define-key rust-mode-map (kbd "C-c C-r") 'rust-run)
(define-key rust-mode-map (kbd "C-c C-b") 'rust-compile)
(define-key rust-mode-map (kbd "C-c C-t") 'rust-test)
(define-key rust-mode-map (kbd "TAB") 'company-indent-or-complete-common)


;;; Company
(define-key company-mode-map (kbd "s-j") 'company-complete)
(define-key company-active-map (kbd "C-h") 'delete-backward-char)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-j") 'company-complete)

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
(define-key dired-mode-map (kbd "C-j") 'dired-find-file)
(define-key dired-mode-map (kbd "C-h") 'dired-unmark-backward)


;;; Comint
(define-key comint-mode-map (kbd "C-j") 'comint-send-input)


;;; Projectile
(define-key projectile-mode-map (kbd "C-x p f") 'projectile-find-file)
(define-key projectile-mode-map (kbd "C-x p 4 f") 'projectile-find-file-other-window)
(define-key projectile-mode-map (kbd "C-x p d") 'projectile-find-dir)
(define-key projectile-mode-map (kbd "C-x p p") 'projectile-switch-project)
(define-key projectile-mode-map (kbd "C-x p s") 'projectile-run-shell)


;;; Vterm
(define-key vterm-mode-map (kbd "C-h") 'vterm-send-backspace)
(define-key global-map (kbd "C-x T") 'vterm-toggle-cd)


;;; GNU Global
;; (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
;; (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
;; (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
;; (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
;; (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
;; (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
;; (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)


;;; Ace-window
(define-key global-map (kbd "C-x o") 'ace-window)


;;; Multiple cursors
(define-key global-map (kbd "C-S-c C-S-c") 'mc/edit-lines)
(define-key global-map (kbd "C->") 'mc/mark-next-like-this)
(define-key global-map (kbd "C-<") 'mc/mark-previous-like-this)
(define-key global-map (kbd "C-c C-<") 'mc/mark-all-like-this)
