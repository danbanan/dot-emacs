;; (use-package alect-themes
;;   :ensure t)

;; (use-package ample-theme
;;   :ensure t)

;; (use-package gruvbox-theme
;;   :ensure t)

;; (use-package github-modern-theme
;;   :ensure t)

;; (use-package solarized-theme
;;   :ensure t)

(use-package leuven-theme
  :ensure t
  :config
  (setq org-fontify-whole-heading-line t))

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

;; (use-package ayu-theme
;;   :ensure t)

(defvar current-theme nil)

;; (defun load-light-theme ()
;;   (interactive)
;;    (load-theme 'solarized-light-high-contrast t)
;;    (setq current-theme 'solarized))

;; (defun load-dark-theme ()
;;   (interactive)
;;    )

(defun load-light-theme ()
  (interactive)
  (cond ((eq (car custom-enabled-themes) 'sanityinc-tomorrow-night)
	 (disable-theme 'sanityinc-tomorrow-night))
	((eq (car custom-enabled-themes) 'deep-blue)
	 (disable-theme 'deep-blue)))
  (setq current-theme 'leuven)
  (load-theme 'leuven))

(defun load-dark-theme ()
  (interactive)
  (cond ((eq (car custom-enabled-themes) 'leuven)
	 (disable-theme 'leuven))
	((eq (car custom-enabled-themes) 'deep-blue)
	 (disable-theme 'deep-blue)))
  ;; (setq current-theme 'sanityinc-tomorrow-night)
  (setq current-theme 'solarized-dark)
  (load-theme current-theme))

(defun load-blue-theme ()
  (interactive)
  (cond ((eq (car custom-enabled-themes) 'leuven)
	 (disable-theme 'leuven))
	((eq (car custom-enabled-themes) 'sanityinc-tomorrow-night)
	 (disable-theme 'sanityinc-tomorrow-night)))
  (setq current-theme 'deep-blue)
  (load-theme 'deep-blue))

(load-light-theme)
