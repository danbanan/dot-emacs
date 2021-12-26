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
(set-frame-font "Monospace 12" nil t)
(set-frame-font "Monospace 11" nil t)
(set-frame-font "Monospace 10" nil t)
;; for 1080p monitor
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

;;; Dired
;; ----------------------------------------------------------------------------- 
(setq dired-dwim-target t)
;; ----------------------------------------------------------------------------- 

;; CUSTOM SET VARIABLES
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(beacon-color "#cc6666")
 '(custom-safe-themes
   '("2c613514f52fb56d34d00cc074fe6b5f4769b4b7f0cc12d22787808addcef12c" "c0a0c2f40c110b5b212eb4f2dad6ac9cac07eb70380631151fa75556b0100063" "21055a064d6d673f666baaed35a69519841134829982cbbb76960575f43424db" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "aba75724c5d4d0ec0de949694bce5ce6416c132bb031d4e7ac1c4f2dbdd3d580" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "039c01abb72985a21f4423dd480ddb998c57d665687786abd4e16c71128ef6ad" default))
 '(fci-rule-color "#373b41")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(hl-sexp-background-color "#efebe9")
 '(package-selected-packages
   '(gnuplot flycheck lsp-mode lsp ayu-theme multiple-cursors visual-fill-column org-tree-slide-pauses org-tree-slide synonymous markdown-toc haskell-mode yasnippet xcscope web-beautify vterm use-package tramp terminal-here spacemacs-theme solarized-theme racer projectile pdf-tools org-bullets magit lsp-java leuven-theme ledger-mode gruvbox-theme github-modern-theme geiser elfeed eglot edit-indirect ebuku counsel company color-theme-sanityinc-tomorrow color-theme-modern color-identifiers-mode auctex ample-theme alect-themes))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68")))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))

;;;* Loading other files
(defun load-config (filename)
  (load (concat "~/.emacs.d/" filename)))

;; Removing theme theme prompt
(load-config "packages.el")
(load-config "themes.el")
(load-config "keybonds.el")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
