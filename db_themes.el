;; THEMES
(unless (package-installed-p 'alect-themes)
  (package-install 'alect-themes))

(unless (package-installed-p 'ample-theme)
  (package-install 'ample-theme))

(unless (package-installed-p 'gruvbox-theme)
  (package-install 'gruvbox-theme))

(unless (package-installed-p 'leuven-theme)
  (package-install 'leuven-theme))

;; Current theme
(load-theme 'leuven t)
