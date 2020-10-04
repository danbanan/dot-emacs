;;; YASnippet - template tool for Emacs
(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))
(add-to-list 'load-path "~/Dropbox/yasnippets/")
(require 'yasnippet)
(yas-global-mode 1)

;;; COMPANY MODE - complete anything
(unless (package-installed-p 'company)
  (package-install 'company))
(require 'company)

;;; LSP - Language Server Protocol
(unless (package-installed-p 'lsp-mode)
  (package-install 'lsp-mode))
(require 'lsp-mode)

;;; MAGIT
(unless (package-installed-p 'magit)
  (package-install 'magit))
(require 'magit)

;; FRAME SETUP
(add-hook 'window-setup-hook
	  (lambda()
	    (setq frame-resize-pixelwise t
		  ns-pop-up-frames nil
		  mac-command-modifier nil
		  select-enable-clipboard t)
	    (set-frame-parameter nil 'undecorated t)
	    (set-frame-position nil 0 0)
	    (set-frame-size nil (- (display-pixel-width) 20) (display-pixel-height) t)
	    (global-visual-line-mode t)))
	  
;; FILE SYSTEM SETUP
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

;; Interactive Do
(setq ido-enable-flex-matching t)
(ido-mode t)

;; LINES SETTINGS
;; Show line numbers
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode t))
;; Set line numbers to visually relative
(setq-default display-line-numbers-type 'visual)
;; Display current column number
(setq column-number-mode t)
;; Adjust line spacing
(setq-default line-spacing 0.5)

;;; Custom keybindings
(setq mac-command-modifier 'super)

;; Org-MODE
(require 'org)
;; Global key shortcuts for org
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
;; Beautify bullets in org-mode
(unless (package-installed-p 'org-bullets)
  (package-install 'org-bullets))
(add-hook 'org-mode-hook (lambda ()
			   (org-bullets-mode 1)
			   (adaptive-wrap-prefix-mode)))
;; Fontify the whole line for headings (with a background color). - Leuven theme
(setq org-fontify-whole-heading-line t)
;; Agenda config
(setq-default org-agenda-files '("~/Dropbox/org/planner"))
(setq org-agenda-skip-scheduled-if-deadline-is-shown 'repeated-after-deadline)
;; Makes sure org is loaded
(setq org-log-done t)
;; Adding habits to org modules
(add-to-list 'org-modules 'org-habits)
;; Org TODO keywords
(setq org-todo-keywords '((sequence "TODO(t)"
				    "IN-PROGRESS(i)"
				    "WAITING(w)"
				    "|"
				    "CANCELLED(c)"
				    "POSTPONED(p)"
				    "DONE(d)")))

;; CALENDER MODE
(add-hook 'calendar-load-hook
	  (lambda ()
	    (calendar-set-date-style 'european)))

;; MARKDOWN-MODE
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))
(require 'markdown-mode)

;; LEDGER-MODE
(unless (package-installed-p 'ledger-mode)
  (package-install 'ledger-mode))
;; Load ledger-mode for '.dat' files
(add-to-list 'auto-mode-alist '("\\.dat\\'" . ledger-mode))

;; CC-MODE
(require 'cc-mode)
;; Making <RET> indent the new line
;; (defun my-make-CR-do-indent ()
;;   (define-key c-mode-base-map "\C-m" 'c-context-line-break))
;; ;; electric-pair-open-newline-between-pairs doesn't do anything for some reason.
;; ;; Using a custom newline instead, taken from Magnar Sveen .emacs
;; (defun new-line-dwim ()
;;   (interactive)
;;   (let ((break-open-pair (or (and (looking-back "{") (looking-at "}"))
;;                              ;; (and (looking-back ">") (looking-at "<"))
;;                              ;; (and (looking-back "(") (looking-at ")"))
;;                              ;; (and (looking-back "\\[") (looking-at "\\]")
;; 			     )))
;;     (newline)
;;     (when break-open-pair
;;       (save-excursion
;;         (newline)
;;         (indent-for-tab-command)))
;;     (indent-for-tab-command)))
;; ;; CC mode hooks
;; (add-hook 'c-initialization-hook 'my-make-CR-do-indent)
;; (add-hook 'c-mode-common-hook
;; 	  (lambda ()
;; 	    ;; Set coding style
;; 	    (setq c-default-style '((awk-mode . "awk")
;; 				    (other . "linux")))
;; 	    (setq indent-tabs-mode nil)
;; 	    (company-mode)))
;; (add-hook 'c-mode-hook
;; 	  (lambda ()
;; 	    (c-set-style "linux")
;; 	    (electric-pair-mode 1)
;; 	    (local-set-key (kbd "<RET>") 'new-line-dwim)))

;; XCSCOPE - cscope interface
(unless (package-installed-p 'xcscope)
  (package-install 'xcscope))
(require 'xcscope)
(cscope-setup)

;; SCHEME DEVELOPMENT: Geiser package
(unless (package-installed-p 'geiser)
  (package-install 'geiser))
;; Set racket path
(setq geiser-racket-binary "/Applications/Racket v7.6/bin/racket")
;; Use Racket version
(setq geiser-active-implementations '(racket))
;; Showing matching parantheses
(show-paren-mode 1)

;; RSS reader
(unless (package-installed-p 'elfeed)
  (package-install 'elfeed))
(global-set-key (kbd "C-x W") 'elfeed)
(setq elfeed-feeds
      '("https://www.uio.no/studier/emner/matnat/ifi/IN5170/h20/exercises/?vrtx=feed"
	"https://www.uio.no/studier/emner/matnat/ifi/IN5170/h20/handouts/?vrtx=feed"
	"https://www.uio.no/studier/emner/matnat/ifi/IN5170/h20/lectures/?vrtx=feed"
	"https://www.uio.no/studier/emner/matnat/ifi/IN5170/h20/obligs/?vrtx=feed"
	"https://www.uio.no/studier/emner/matnat/ifi/IN5170/h20/beskjeder/?vrtx=feed"
	"https://www.uio.no/studier/emner/matnat/ifi/IN5020/h20/beskjeder/?vrtx=feed"
	"https://www.uio.no/studier/emner/matnat/ifi/IN5070/h20/beskjeder/?vrtx=feed"))

;; JAVA DEVELOPMENT
;; eclim - not good
;; (unless (package-installed-p 'eclim)
;;   (package-install 'eclim))
;; (require 'eclim)
;; (setq eclimd-executable "/Users/danrachou/.p2/pool/plugins/org.eclim_2.8.0/bin/eclimd")
;; (setq eclim-executable "/Users/danrachou/.p2/pool/plugins/org.eclim_2.8.0/bin/eclim")
;; lsp
(unless (package-installed-p 'lsp-java)
  (package-install 'lsp-java))
(require 'lsp-java)
(setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
(add-hook 'java-mode-hook #'lsp)
(electric-pair-mode 1)

;; Assembler DEVELOPMENT: gas-mode
;; (require 'gas-mode)
;; (add-to-list 'auto-mode-alist '("\\.S\\'" . gas-mode))

;; Nasm-mode
;; (unless (package-installed-p 'nasm-mode)
;;   (package-install 'nasm-mode))

;; Asm86 mode
;; (autoload 'asm86-mode "~/.emacs/non-elpa/emacs86/asm86-mode.elc")

;; ;; Make Emacs load Asm86 mode for .asm files.
;; (setq auto-mode-alist
;;       (append '(("\\.asm\\'" . asm86-mode) ("\\.inc\\'" . asm86-mode))
;; 	      auto-mode-alist))

;; ;; Enabling syntax highlighting.
;; (add-hook 'asm86-mode 'turn-on-font-lock)

;; "Recommended" color scheme.
;; (cond ((fboundp 'global-font-lock-mode)
;;        ;; Customize face attributes
;;        (setq font-lock-face-attributes
;;              ;; Symbol-for-Face Foreground Background Bold Italic Underline
;;              '((font-lock-comment-face       "DarkGreen")
;;                (font-lock-string-face        "Sienna")
;;                (font-lock-keyword-face       "RoyalBlue")
;;                (font-lock-function-name-face "Red")
;;                (font-lock-variable-name-face "Black")
;;                (font-lock-type-face          "Blue")
;;                (font-lock-constant-face      "Purple")
;;                ))
;;        ;; Load the font-lock package.
;;        (require 'font-lock)))


;; Function to set frame to half screen
(defun halfscreen-frame ()
  (interactive)
  (set-frame-size nil (- (/ (display-pixel-width) 2) 20)
		  (display-pixel-height) t)
  (revert-buffer nil 1))

;; Function to set frame to full screen
(defun fullscreen-frame ()
  (interactive)
  (set-frame-size nil (- (display-pixel-width) 20) (display-pixel-height) t))

;; (defun my-asm-mode-hook ()
;;   ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
;;   (local-unset-key (vector asm-comment-char))
;;   ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
;;   (setq tab-always-indent (default-value 'tab-always-indent)))

;; (add-hook 'asm-mode-hook #'my-asm-mode-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "e1d09f1b2afc2fed6feb1d672be5ec6ae61f84e058cb757689edb669be926896" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "123a8dabd1a0eff6e0c48a03dc6fb2c5e03ebc7062ba531543dfbce587e86f2a" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" default)))
 '(ledger-reports
   (quote
    (("netw" "ledger balance Assets Liabilities")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)"))))
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(package-selected-packages
   (quote
    (adaptive-wrap xcscope ledger-mode centered-window org-bullets org ample-theme alec-themes geiser markdown-mode dracula-theme auctex alect-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
