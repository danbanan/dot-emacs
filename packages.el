;;;* YASnippet - template tool for Emacs
(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))
(add-to-list 'load-path "~/Dropbox/yasnippets/")
(require 'yasnippet)

;;;* COMPANY MODE - complete anything
(unless (package-installed-p 'company)
  (package-install 'company))
(require 'company)

;;;* LSP - Language Server Protocol
(unless (package-installed-p 'lsp-mode)
  (package-install 'lsp-mode))
(require 'lsp-mode)

;;;* Magit
(unless (package-installed-p 'magit)
  (package-install 'magit))
(require 'magit)
;; Fixes SSH passphrases in Windows by storing passphrase in agent
(when (and (string-equal system-type "windows-nt")
	   (not (package-installed-p 'ssh-agency)))
  (package-install 'ssh-agency))

;;;* Interactive Do
;; (setq ido-enable-flex-matching t)
;; (ido-mode t)

;;;* Minibuffer completion
;; Ivy - generic completion mechanism
(unless (package-installed-p 'ivy)
  (package-install 'ivy))
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;;; Counsel - collection of Ivy-enhanced versions of common Emacs commands
; --------------------------------------------------------------------
(unless (package-installed-p 'counsel)
  (package-install 'counsel))
; --------------------------------------------------------------------

;;; Global key bindings
; --------------------------------------------------------------------
(global-set-key (kbd "C-x j f") 'counsel-file-jump)
(global-set-key (kbd "C-x j d") 'counsel-dired-jump)
; --------------------------------------------------------------------

;;;* Lines Settings
;; Show line numbers
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode t))
;; Set line numbers to visually relative
(setq-default display-line-numbers-type 'visual)
;; Display current column number
(setq column-number-mode t)
;; Adjust line spacing
(setq-default line-spacing 0.5)

;;;* Lisp mode
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (outline-minor-mode t)
	    (setq outline-regexp ";;;\\*+")
	    (local-set-key (kbd "C-c C-c") 'outline-hide-entry)
	    (local-set-key (kbd "C-c C-e") 'outline-show-entry)
	    (local-set-key (kbd "C-c C-o") 'counsel-outline)
	    (electric-pair-mode t)
	    (outline-hide-body)))

;;;* Org mode
;; agenda config
(setq-default org-agenda-files '("~/Dropbox/org/planner"))
(setq org-agenda-skip-scheduled-if-deadline-is-shown 'repeated-after-deadline)
;; Makes sure org is loaded
(setq org-log-done t)
;; Fix clock report format in agenda
(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4))
;; Global key shortcuts for org
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
;; Beautify bullets in org-mode
(unless (package-installed-p 'org-bullets)
  (package-install 'org-bullets))
(add-hook 'org-mode-hook (lambda ()
			   (org-bullets-mode 1)
			   ))
;; Fontify the whole line for headings (with a background color). - Leuven theme
(setq org-fontify-whole-heading-line t)

;; Org TODO keywords
(setq org-todo-keywords '((sequence "TODO(t)"
				    "IN-PROGRESS(i)"
				    "WAITING(w)"
				    "|"
				    "CANCELLED(c)"
				    "POSTPONED(p)"
				    "DONE(d)")))
;; Make Org commands work on regions
(setq org-loop-over-headlines-in-active-region 'start-level)

;;;* CALENDER MODE
(add-hook 'calendar-load-hook
	  (lambda ()
	    (calendar-set-date-style 'european)))

;;;* MARKDOWN-MODE
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))
(require 'markdown-mode)

;;;* LEDGER-MODE
(unless (package-installed-p 'ledger-mode)
  (package-install 'ledger-mode))
;; Load ledger-mode for '.dat' files
(add-to-list 'auto-mode-alist '("\\.dat\\'" . ledger-mode))

;;;* CC-MODE
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
;;;* XCSCOPE - cscope interface
(unless (package-installed-p 'xcscope)
  (package-install 'xcscope))
(require 'xcscope)
(cscope-setup)

;;;* SCHEME DEVELOPMENT: Geiser package
(unless (package-installed-p 'geiser)
  (package-install 'geiser))
;; Set racket path
(setq geiser-racket-binary "/Applications/Racket v7.6/bin/racket")
;; Use Racket version
(setq geiser-active-implementations '(racket))
;; Showing matching parantheses
(show-paren-mode 1)

;;;* RSS reader
(unless (package-installed-p 'elfeed)
  (package-install 'elfeed))
(global-set-key (kbd "C-x W") 'elfeed)
(setq elfeed-feeds
      '(;; School stuff
	"https://www.uio.no/studier/emner/matnat/ifi/INF5110/v21/?vrtx=feed"
	"https://www.uio.no/studier/emner/matnat/ifi/INF5110/v21/exercises/?vrtx=feed"
	"https://www.uio.no/studier/emner/matnat/ifi/INF5110/v21/handouts/?vrtx=feed"
	"https://www.uio.no/studier/emner/matnat/ifi/INF5110/v21/obligs/?vrtx=feed"
	"https://www.uio.no/studier/emner/matnat/ifi/INF5110/v21/slides/?vrtx=feed"
	"https://www.uio.no/studier/emner/matnat/ifi/IN5050/v21/index.html?vrtx=feed"
	;; Emacs stuff
	"http://pragmaticemacs.com/feed/"))

;;;* JAVA DEVELOPMENT
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
(defun db-java-hook ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (set-fill-column 100)
  (local-set-key "M-tab" 'company-complete)
  (electric-pair-mode 1))
(add-hook 'java-mode-hook #'lsp)
(add-hook 'java-mode-hook 'db-java-hook)

;;;* Assembler DEVELOPMENT: gas-mode
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
;; (defun my-asm-mode-hook ()
;;   ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
;;   (local-unset-key (vector asm-comment-char))
;;   ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
;;   (setq tab-always-indent (default-value 'tab-always-indent)))
;; (add-hook 'asm-mode-hook #'my-asm-mode-hook)


;;;* Frame size editor
;; Function to set frame to half screen
;; (defun halfscreen-frame ()
;;   (interactive)
;;   (set-frame-size nil (- (/ (display-pixel-width) 2) 20)
;; 		  (display-pixel-height) t)
;;   (revert-buffer nil 1))
;; Function to set frame to full screen
;; (defun fullscreen-frame ()
;;   (interactive)
;;   (set-frame-size nil (- (display-pixel-width) 20) (display-pixel-height) t))

;;;* Ebuku - bookmark manager
(unless (package-installed-p 'ebuku)
  (package-install 'ebuku))
;;;* rust-mode
(unless (package-installed-p 'rust-mode)
  (package-install 'rust-mode))
;; racer-mode - completion, find definition and describe functions and types in rust-mode
(unless (package-installed-p 'racer)
  (package-install 'racer))
(add-hook 'racer-mode-hook #'eldoc-mode)
(require 'rust-mode)
;; The Rust style guide recommends spaces rather than tabs for indentation.
(add-hook 'rust-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (company-mode)
	    (racer-mode)))
(define-key rust-mode-map (kbd "C-c C-c") #'rust-check)
(define-key rust-mode-map (kbd "C-c C-r") #'rust-run)
(define-key rust-mode-map (kbd "C-c C-b") #'rust-compile)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
