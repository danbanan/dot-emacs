(use-package alect-themes
  :ensure t)

(use-package ample-theme
  :ensure t)

(use-package gruvbox-theme
  :ensure t)

(use-package github-modern-theme
  :ensure t)

(use-package solarized-theme
  :ensure t)

(use-package leuven-theme
  :ensure t
  :config
  (setq org-fontify-whole-heading-line t))

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

(defun load-blue-theme ()
  (interactive)
  (load-theme 'deep-blue t)
  (set-face-attribute 'line-number nil :foreground "dark grey" :background "#102e4e")
  (setq current-theme 'color))

(load-blue-theme)
