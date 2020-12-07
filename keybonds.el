;;;* Apple modifier keys
(setq mac-option-modifier 'meta
      mac-command-modifier 'super)
;;;* Norwegian keys
(define-key key-translation-map (kbd "s-'") (kbd "æ"))
(define-key key-translation-map (kbd "s-o") (kbd "ø"))
(define-key key-translation-map (kbd "s-a") (kbd "å"))
(define-key key-translation-map (kbd "s-\"") (kbd "Æ"))
(define-key key-translation-map (kbd "s-O") (kbd "Ø"))
(define-key key-translation-map (kbd "s-A") (kbd "Å"))
