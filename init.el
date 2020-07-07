;; BASIC SETUP
(require 'package)

(add-to-list 'package-archives
	'("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (file-exists-p (expand-file-name
			 (concat package-user-dir
				 "/archives/MELPA/")))
  (package-refresh-contents))

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

;; CENTER WINDOW: activated only on single windows
(unless (package-installed-p 'centered-window)
  (package-install 'centered-window))
;; Enable centered window
(centered-window-mode t)


;; FRAME SETUP
(add-hook 'window-setup-hook
	  (lambda()
	    (setq frame-resize-pixelwise t
		  ns-pop-up-frames nil
		  mac-command-modifier nil
		  select-enable-clipboard t)
	    (set-face-attribute 'default nil :family "Consolas" :height 120)
	    (tool-bar-mode -1)
	    (scroll-bar-mode -1)
	    (set-frame-parameter nil 'undecorated t)
	    (set-frame-position nil 0 0)
	    (set-frame-size nil (- (display-pixel-width) 34) (- (display-pixel-height) 70) t)
	    (global-visual-line-mode t)))

	    

;; FILE SYSTEM SETUP
;; Disables back-up files, i.e. all files starting with '~'.
(setq make-backup-files nil)
;; Adds the load-path to my personal elisp library directory, used for installing packages manually.
(add-to-list 'load-path "~/.emacs.d/lisp/")
;; 'ls' command in Mac OSX does not support '--dired' flag.
(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))


;; LINES SETTINGS
;; Show line numbers
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode t))
;; Set line numbers to visually relative
(setq-default display-line-numbers-type 'visual)
;; Adjust line spacing
(setq-default line-spacing 0.5)


;; ORG-MODE
(require 'org)
;; Global key shortcuts for org
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
;; Makes sure org is loaded
(setq org-log-done t)
;; Beautify bullets in org-mode
(unless (package-installed-p 'org-bullets)
  (package-install 'org-bullets))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; Fontify the whole line for headings (with a background color). - Leuven theme
(setq org-fontify-whole-heading-line t)
;; Agenda config
(setq org-agenda-files '("c:/Users/dra/Dropbox/org/planner/"))
(setq org-agenda-skip-scheduled-if-deadline-is-shown 'repeated-after-deadline)

;; CC-MODE
(require 'cc-mode)

;; Set coding style
(setq c-default-style '((java-mode . "java")
			(cc-mode . "k&r")
			(other . "gnu")))

;; Showing matching parantheses
(show-paren-mode 1)



;; Function to set frame to half screen
(defun halfscreen-frame ()
  (set-frame-size nil (- (/ (display-pixel-width) 2) 20)
		  (display-pixel-height) t))

;; Function to set frame to full screen
(defun fullscreen-frame ()
  (set-frame-size nil (- (display-pixel-width) 20) (display-pixel-height)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "e1d09f1b2afc2fed6feb1d672be5ec6ae61f84e058cb757689edb669be926896" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "123a8dabd1a0eff6e0c48a03dc6fb2c5e03ebc7062ba531543dfbce587e86f2a" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" default)))
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (centered-window org-bullets org ample-theme alec-themes geiser markdown-mode dracula-theme auctex alect-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
