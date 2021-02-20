;;;* Basic setup
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (file-exists-p (expand-file-name
			 (concat package-user-dir
				 "/archives/MELPA/")))
  (package-refresh-contents))

;; Set font
(cond ((string-equal system-type "darwin")
       (add-to-list 'default-frame-alist
		    '(font . "Iosevka Extended-13.5")))
       ((string-equal system-type "gnu/linux")
	(add-to-list 'default-frame-alist
;;		     '(font . "Monospace-11")))
		     '(font . "Iosevka Extended-10.8")))
       (else (add-to-list 'default-frame-alist
			  '(font . "Iosevka Light Extended-10.5"))))
;; Disable menu bar
(menu-bar-mode -1)
;; Disable tool bar
(tool-bar-mode -1)
;; Disable scroll bar
(scroll-bar-mode -1)
;; Sort completions vertically
(setq completions-format 'vertical)

;; Disable start-up screen
;; ------------------------------------------------------------------
(setq inhibit-startup-message t)
;; -----------------------------------------------------------------

;;;* File system setup
;; Disables back-up files, i.e. all files starting with '~'.
(setq make-backup-files nil)
;; Adds the load-path to my personal elisp library directory, used for installing packages manually.
(add-to-list 'load-path "~/.emacs.d/lisp/")
;; Use 'gls' command in Mac OSX for dired
(when (string= system-type "darwin")       
  (setq insert-directory-program "gls"
	dired-use-ls-dired t))
;; Sort directories first and byte, kilobyte, megabyte ... suffixes.
;; 'coreutils' must be installed
(setq dired-listing-switches "-ahl --group-directories-first")

;; Set default directory on start-up
; --------------------------------------------------------------------
(setq default-directory "~/Dropbox/")
; --------------------------------------------------------------------
;;;* Frame setup
(add-hook 'window-setup-hook
	  (lambda()
	    (setq frame-resize-pixelwise t)
	    (setq ns-pop-up-frames nil)
	    (setq select-enable-clipboard t)
	    (set-frame-parameter nil 'undecorated t)
	    (set-frame-position nil 0 0)
	    (global-visual-line-mode t)
	    (cond ((string-equal system-type "darwin")
		   (set-frame-size nil (- (display-pixel-width) 20) (display-pixel-height) t))
		  ((string-equal system-type "gnu/linux")
		   (set-frame-size nil (- (display-pixel-width) 16) (display-pixel-height) t))
		  ((string-equal system-type "windows-nt")
		   (set-frame-size nil (- (display-pixel-width) 32) (- (display-pixel-height) 40) t)))))
;;;* Shell settings
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;;;* Loading other files
(defun load-config (filename)
  (load (concat "~/.emacs.d/" filename)))

(load-config "themes.el")
(load-config "packages.el")
(load-config "keybonds.el")

;; CUSTOM SET VARIABLES
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "e1d09f1b2afc2fed6feb1d672be5ec6ae61f84e058cb757689edb669be926896" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "123a8dabd1a0eff6e0c48a03dc6fb2c5e03ebc7062ba531543dfbce587e86f2a" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" default))
 '(ledger-reports
   '(("netw" "ledger balance Assets Liabilities")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(edit-indirect vterm terminal-here tramp use-package racer racer-mode rust-mode ebuku ssh-agency magit lsp-mode eclim emacs-eclim adaptive-wrap xcscope ledger-mode centered-window org-bullets ample-theme alec-themes geiser markdown-mode dracula-theme auctex alect-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
