(use-package alect-themes
  :ensure t)

(unless (package-installed-p 'ample-theme)
  (package-install 'ample-theme))

(unless (package-installed-p 'gruvbox-theme)
  (package-install 'gruvbox-theme))

(use-package github-modern-theme
  :ensure t)

(use-package solarized-theme
  :ensure t)

(use-package leuven-theme
  :ensure t
  :config
  (setq org-fontify-whole-heading-line t))

;; (use-package spacemacs-theme
;;   :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(defvar current-theme nil)

;; (defun load-light-theme ()
;;   (interactive)
;;    (load-theme 'solarized-light-high-contrast t)
;;    (setq current-theme 'solarized))

;; (defun load-dark-theme ()
;;   (interactive)
;;    (load-theme 'solarized-dark t)
;;    (setq current-theme 'solarized))

(defun load-light-theme ()
  (interactive)
   (load-theme 'sanityinc-tomorrow-day t)
   (setq current-theme 'tomorrow))

(defun load-dark-theme ()
  (interactive)
   (load-theme 'sanityinc-tomorrow-night t)
   (setq current-theme 'tomorrow))

(load-light-theme)
