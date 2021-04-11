;; THEMES
(unless (package-installed-p 'alect-themes)
  (package-install 'alect-themes))

(unless (package-installed-p 'ample-theme)
  (package-install 'ample-theme))

(unless (package-installed-p 'gruvbox-theme)
  (package-install 'gruvbox-theme))

(use-package github-modern-theme
  :ensure t)

(use-package leuven-theme
  :ensure t
  :init (load-theme 'solarized-dark t))
