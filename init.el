;; BASIC SETUP
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(unless (file-exists-p (expand-file-name
			 (concat package-user-dir
				 "/archives/MELPA/")))
  (package-refresh-contents))


;; Install theme package
(unless (package-installed-p 'alect-themes)
  (package-install 'alect-themes))


;; WINDOW SETUP
(add-hook 'window-setup-hook
	  (lambda()
	    (setq frame-resize-pixelwise t
		  ns-pop-up-frames nil
		  ;; mac-option-modifier nil
		  mac-command-modifier nil
		  select-enable-clipboard t)
	    (set-face-attribute 'default nil :family "Anonymous Pro" :height 170)
	    (tool-bar-mode -1)
	    (scroll-bar-mode -1)
	    (set-frame-position nil 0 -24)
	    (fullscreen-frame)
	    ;; Awesome color theme
	    (load-theme 'alect-light)))


;; Set up Geiser for Scheme developement
(unless (package-installed-p 'geiser)
  (package-install 'geiser))

;; Set racket path
(setq geiser-racket-binary "/Applications/Racket v7.6/bin/racket")

;; Use Racket version
(setq geiser-active-implementations '(racket))

;; Showing matching parantheses
(show-paren-mode 1)

;; Loads supercool old school color theme
;; TODO: Get rid of annyoing startup questions. Install manually.


;; Function to set frame to half screen
(defun halfscreen-frame ()
  (set-frame-size nil (- (/ (display-pixel-width) 2) 20)
		  (display-pixel-height) t))

;; Function to set frame to full screen
(defun fullscreen-frame ()
  (set-frame-size nil (- (display-pixel-width) 20) (display-pixel-height)))

;; (defun my-asm-mode-hook ()
;;   ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
;;   (local-unset-key (vector asm-comment-char))
;;   ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
;;   (setq tab-always-indent (default-value 'tab-always-indent)))

;; (add-hook 'asm-mode-hook #'my-asm-mode-hook)

(require 'cc-mode)
;; Change indentation level in C major mode
(setq-default c-basic-offset 4)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" default)))
 '(package-selected-packages
   (quote
    (alec-themes geiser markdown-mode dracula-theme auctex alect-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
