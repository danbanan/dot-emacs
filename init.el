(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq warning-minimum-level :error)

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
;; (cond ((string-equal system-type "darwin")
;;        ;; (set-frame-font "Iosevka 15" nil t)
;;        (set-frame-font "Fira Code 14" nil t))
;;        ((string-equal system-type "gnu/linux")
;; 	;; (set-frame-font "Iosevka Heavy 12")
;; 	(set-frame-font "Fira Code 11" nil t))
;;        (else (add-to-list 'default-frame-alist
;; 			  '(font . "Iosevka Light Extended-10.5"))))
(set-frame-font
 "-JB-JetBrains Mono-normal-normal-normal-*-19-*-*-*-m-0-iso10646-1") ; 16:18 screen
;; (set-frame-font
;;  "-JB-JetBrains Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
					; regular laptop screen
;; (set-frame-font "Monospace 8" nil t)  ; 4K monitor
;; (set-frame-font "Monospace 19" nil t) ;; Projector
(set-frame-font "Monospace 9" nil t) ; 1080p monitor
;; -----------------------------------------------------------------------------

;; Disable menu, tool, and scroll bar
;; -----------------------------------------------------------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; -----------------------------------------------------------------------------

;; Split windows evenly
(setq window-combination-resize t)

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
	    (setq split-height-threshold nil) ; always split horizontaly
	    (setq split-width-threshold 0)
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
;; Install colors in shell mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; To get colored output from shell
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
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

;;;* Dired
;; ----------------------------------------------------------------------------- 
(setq dired-dwim-target t)
;; ----------------------------------------------------------------------------- 

;;;* Compilation
;; -----------------------------------------------------------------------------
(setq compilation-scroll-output t)
;; -----------------------------------------------------------------------------

;;;* TRAMP
;; -----------------------------------------------------------------------------
(setq tramp-verbose 10)
;; -----------------------------------------------------------------------------

(customize-set-variable 'next-screen-context-lines 25)

;; (setq split-height-threshold nil)
;; (setq split-width-threshold 160)

(setq custom-file (concat user-emacs-directory "custom.el"))

;;;* Loading other files
(defun load-config (filename)
  (load (concat user-emacs-directory filename)))

;; Removing theme theme prompt
(load-config "packages.el")
(load-config "themes.el")
(load-config "keybonds.el")
