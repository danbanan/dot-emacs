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

;; Font settings
;; -----------------------------------------------------------------------------
(cond ((string-equal system-type "darwin")
       (add-to-list 'default-frame-alist
		    '(font . "Iosevka Extended-15.5")))
       ((string-equal system-type "gnu/linux")
	(add-to-list 'default-frame-alist
;;		     '(font . "Monospace-11")))
		     '(font . "Iosevka Heavy Extended-10.8")))
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
	    ;; (cond ((string-equal system-type "darwin")
	    ;; 	   (set-frame-size nil (- (display-pixel-width) 20) (display-pixel-height) t))
	    ;; 	  ((string-equal system-type "gnu/linux")
	    ;; 	   (set-frame-size nil (- (display-pixel-width) 16) (display-pixel-height) t))
	    ;; 	  ((string-equal system-type "windows-nt")
	    ;; 	   (set-frame-size nil (- (display-pixel-width) 32) (- (display-pixel-height) 40) t)))
	    (toggle-frame-maximized)))

(defun ask-before-closing ()
  "Close only if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to close this frame? "))
      (save-buffers-kill-emacs)
    (message "Canceled frame close")))

(define-key global-map (kbd "C-x C-c") 'ask-before-closing)
(define-key global-map (kbd "C-z") 'ignore)
(define-key global-map (kbd "C-x C-z") 'ignore)

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
	    (outline-minor-mode t)
	    (setq outline-regexp ";;;\\*+")
	    (local-set-key (kbd "C-c C-h") 'outline-hide-entry)
	    (local-set-key (kbd "C-c C-s") 'outline-show-entry)
	    (local-set-key (kbd "C-c C-o") 'counsel-outline)
	    (electric-pair-mode t)))
;; -----------------------------------------------------------------------------

;; CUSTOM SET VARIABLES
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#757575" "#CD5542" "#4A8F30" "#7D7C21" "#4170B3" "#9B55C3" "#68A5E9" "gray43"])
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#11948b")
 '(cua-normal-cursor-color "#596e76")
 '(cua-overwrite-cursor-color "#a67c00")
 '(cua-read-only-cursor-color "#778c00")
 '(custom-safe-themes
   '("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "3f5f69bfa958dcf04066ab2661eb2698252c0e40b8e61104e3162e341cee1eb9" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "8feca8afd3492985094597385f6a36d1f62298d289827aaa0d8a62fe6889b33c" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "e1d09f1b2afc2fed6feb1d672be5ec6ae61f84e058cb757689edb669be926896" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "123a8dabd1a0eff6e0c48a03dc6fb2c5e03ebc7062ba531543dfbce587e86f2a" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" default))
 '(fci-rule-color "#f4eedb")
 '(highlight-changes-colors '("#c42475" "#5e65b6"))
 '(highlight-symbol-colors
   '("#ed7ddb24b29e" "#cd82e29fd17d" "#fc9acae0b444" "#d974d4beddd6" "#df08dfc6b349" "#f76ccd6eaf2a" "#d132db92e15a"))
 '(highlight-symbol-foreground-color "#5d737a")
 '(highlight-tail-colors
   '(("#f4eedb" . 0)
     ("#a8b84b" . 20)
     ("#66c1b3" . 30)
     ("#6fa5e7" . 50)
     ("#d6a549" . 60)
     ("#ed6e3e" . 70)
     ("#f46495" . 85)
     ("#f4eedb" . 100)))
 '(hl-bg-colors
   '("#d6a549" "#ed6e3e" "#ff6243" "#f46495" "#837bdf" "#6fa5e7" "#66c1b3" "#a8b84b"))
 '(hl-fg-colors
   '("#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9"))
 '(hl-paren-colors '("#11948b" "#a67c00" "#007ec4" "#5e65b6" "#778c00"))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(ledger-reports
   '(("netw" "ledger balance Assets Liabilities")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(lsp-ui-doc-border "#5d737a")
 '(nrepl-message-colors
   '("#cc1f24" "#bb3e06" "#a67c00" "#4f6600" "#a8b84b" "#005797" "#11948b" "#c42475" "#5e65b6"))
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(color-theme-sanityinc-tomorrow spacemacs-theme eglot pdf-tools web-beautify solarized-theme color-identifiers-mode github-modern-theme github-modern edit-indirect vterm terminal-here tramp use-package racer racer-mode rust-mode ebuku ssh-agency magit lsp-mode eclim emacs-eclim adaptive-wrap xcscope ledger-mode centered-window org-bullets ample-theme alec-themes geiser markdown-mode dracula-theme auctex alect-themes))
 '(pdf-view-midnight-colors '("#655370" . "#fbf8ef"))
 '(pos-tip-background-color "#f4eedb")
 '(pos-tip-foreground-color "#5d737a")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#778c00" "#f4eedb" 0.2))
 '(term-default-bg-color "#fffce9")
 '(term-default-fg-color "#596e76")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#cc1f24")
     (40 . "#bbbc5a0718b8")
     (60 . "#b1cf6c680f6e")
     (80 . "#a67c00")
     (100 . "#980682770000")
     (120 . "#906085330000")
     (140 . "#887187c90000")
     (160 . "#802a8a3b0000")
     (180 . "#778c00")
     (200 . "#69798ef83f8d")
     (220 . "#5f60904453a3")
     (240 . "#51b991a1669e")
     (260 . "#3d7993107928")
     (280 . "#11948b")
     (300 . "#1b098bdaa289")
     (320 . "#1963876fadf4")
     (340 . "#129982fcb95b")
     (360 . "#007ec4")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#fffce9" "#f4eedb" "#990001" "#cc1f24" "#4f6600" "#778c00" "#785700" "#a67c00" "#005797" "#007ec4" "#93004d" "#c42475" "#006d68" "#11948b" "#596e76" "#88999b"))
 '(xterm-color-names
   ["#f4eedb" "#cc1f24" "#778c00" "#a67c00" "#007ec4" "#c42475" "#11948b" "#002b37"])
 '(xterm-color-names-bright
   ["#fffce9" "#bb3e06" "#98a6a6" "#88999b" "#596e76" "#5e65b6" "#5d737a" "#00212b"]))

;;;* Loading other files
(defun load-config (filename)
  (load (concat "~/.emacs.d/" filename)))

;; Removing theme theme prompt
(load-config "themes.el")
(load-config "packages.el")
(load-config "keybonds.el")


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
