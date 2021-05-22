;;;* PACKAGE MANAGER

;; Package repository with 'package' and adding repository source
;; -----------------------------------------------------------------------------
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; -----------------------------------------------------------------------------

;; I don't know what this really does... -> I don't think it's necessary either
;; -----------------------------------------------------------------------------
;; (unless (file-exists-p
;; 	 (expand-file-name (concat package-user-dir "/archives/MELPA/")))
;;   (package-refresh-contents))
;; -----------------------------------------------------------------------------

;; Adds the load-path to my personal elisp library directory, used for
;; installing packages manually.
;; -----------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/lisp/")
;; -----------------------------------------------------------------------------

;;;* STARTUP SCREEN

;; Startup message displaying the time of starting emacs
;; -----------------------------------------------------------------------------
(defun db/display-startup-time ()
  (message "Emacs loaded in %s seconds with %d garbage collections."
	   (format "%.3f" (float-time
			   (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'db/display-startup-time)
;; -----------------------------------------------------------------------------

;; TODO, start eshell upon startup

;;; Font settings
;; -----------------------------------------------------------------------------
(cond ((string-equal system-type "darwin")
       ;; (set-frame-font "Iosevka 15" nil t)
       (set-frame-font "Fira Code 14" nil t))
       ((string-equal system-type "gnu/linux")
	;; (set-frame-font "Iosevka Heavy 12")
	(set-frame-font "Fira Code 11" nil t))
       (else (add-to-list 'default-frame-alist
			  '(font . "Iosevka Light Extended-10.5"))))
;; -----------------------------------------------------------------------------

;; Disable menu, tool, and scroll bar
;; -----------------------------------------------------------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; -----------------------------------------------------------------------------

;; Sort completions vertically
;; -----------------------------------------------------------------------------
(setq completions-format 'vertical)
;; -----------------------------------------------------------------------------

;; Disable start-up screen
;; ------------------------------------------------------------------
(setq inhibit-startup-message t)
;; -----------------------------------------------------------------

;;;* FILE SYSTEM SETUP

;; Disables back-up files, i.e. all files starting with '~'.
;; -----------------------------------------------------------------------------
(setq make-backup-files nil)
;; -----------------------------------------------------------------------------

;; Use 'gls' command in Mac OSX for dired
(when (string= system-type "darwin")       
  (setq insert-directory-program "gls"
	dired-use-ls-dired t))
;; Sort directories first and byte, kilobyte, megabyte ... suffixes.
;; 'coreutils' must be installed
(setq dired-listing-switches "-ahl --group-directories-first")
;; -----------------------------------------------------------------------------

;; Set default directory on start-up
;; -----------------------------------------------------------------------------
(setq default-directory "~/Dropbox/")
;; -----------------------------------------------------------------------------

;;;* FRAME SETUP
;; -----------------------------------------------------------------------------
(add-hook 'window-setup-hook
	  (lambda()
	    (setq frame-resize-pixelwise t)
	    (setq ns-pop-up-frames nil)
	    (setq select-enable-clipboard t)
	    (set-frame-parameter nil 'undecorated t)
	    (set-frame-position nil 0 0)
	    (global-visual-line-mode t)
	    ;; showing matching parantheses
	    (show-paren-mode 1)	    	
	    (toggle-frame-maximized)))

(defun ask-before-closing ()
  "Close only if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to close this frame? "))
      (save-buffers-kill-emacs)
    (message "Canceled frame close")))


;;;* SHELL SETTINGS
;; -----------------------------------------------------------------------------
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
;; -----------------------------------------------------------------------------

;;;* ELISP
;; -----------------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq outline-regexp ";;;\\*+")
	    (outline-minor-mode t)
	    (electric-pair-mode t)))
;; -----------------------------------------------------------------------------

;; CUSTOM SET VARIABLES
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "039c01abb72985a21f4423dd480ddb998c57d665687786abd4e16c71128ef6ad" default))
 '(package-selected-packages
   '(yasnippet xcscope web-beautify vterm use-package tramp terminal-here spacemacs-theme solarized-theme racer projectile pdf-tools org-bullets magit lsp-java leuven-theme ledger-mode gruvbox-theme github-modern-theme geiser elfeed eglot edit-indirect ebuku counsel company color-theme-sanityinc-tomorrow color-theme-modern color-identifiers-mode auctex ample-theme alect-themes)))

;;;* Loading other files
(defun load-config (filename)
  (load (concat "~/.emacs.d/" filename)))

;; Removing theme theme prompt
(load-config "packages.el")
(load-config "themes.el")
(load-config "keybonds.el")
(load-config "setups.el")


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
