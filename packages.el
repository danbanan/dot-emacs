(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;; Latex
(unless (package-installed-p 'auctex)
  (package-refresh-contents)
  (package-install 'auctex))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(when (string-equal system-type "darwin")
  (add-to-list 'exec-path "/opt/texlive/2021/bin/x86_64-linux"))

;;; YASnippet - template tool for Emacs
;; (unless (package-installed-p 'yasnippet)
;;   (package-install 'yasnippet))
;; (add-to-list 'load-path "~/Dropbox/yasnippets/")
;; (require 'yasnippet)

;;; COMPANY MODE - complete anything
(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0.3
	company-async-timeout 15
	company-tooltip-align-annotations t))

;;; Magit
(unless (package-installed-p 'magit)
  (package-install 'magit))
(require 'magit)
;; Fixes SSH passphrases in Windows by storing passphrase in agent
(when (and (string-equal system-type "windows-nt")
	   (not (package-installed-p 'ssh-agency)))
  (package-install 'ssh-agency))

;;; Minibuffer completion
;; Ivy - generic completion mechanism
(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))
(define-key ivy-minibuffer-map (kbd "M-j") 'ivy-done)
(define-key ivy-minibuffer-map (kbd "C-S-d") 'ivy-backward-delete-char)
(define-key ivy-minibuffer-map (kbd "<return>") 'ignore)
(define-key ivy-minibuffer-map (kbd "<backspace>") 'ignore)

;;; Counsel - collection of Ivy-enhanced versions of common Emacs commands
; --------------------------------------------------------------------
(use-package counsel
  :ensure t)
; --------------------------------------------------------------------

;;; Global key bindings
; --------------------------------------------------------------------
(global-set-key (kbd "C-x j f") 'counsel-file-jump)
(global-set-key (kbd "C-x j d") 'counsel-dired-jump)
; --------------------------------------------------------------------

;;; Lines Settings
;; Show line numbers
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode t))
;; Set line numbers to visually relative
(setq-default display-line-numbers-type 'visual)
;; Display current column number
(setq column-number-mode t)
;; Adjust line spacing
(setq-default line-spacing 0.5)

;;; Lisp mode
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (outline-minor-mode t)
	    (company-mode)
	    (local-set-key (kbd "<C-tab>") 'counsel-company)
	    (local-set-key (kbd "C-c SPC") 'counsel-outline)
	    (electric-pair-mode t)))

;;; Org mode
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
			   (outline-minor-mode t)
			   (local-set-key (kbd "C-c SPC") 'counsel-outline)
			   (outline-hide-sublevels 1)))
;; Fontify the whole line for headings (with a background color). - Leuven theme
(setq org-fontify-whole-heading-line t)
;; Fontify bold, italixs and underlined text without the pre-symbols
(setq org-hide-emphasis-markers t)

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

;;; CALENDER MODE
(add-hook 'calendar-load-hook
	  (lambda ()
	    (calendar-set-date-style 'european)))

;;; MARKDOWN-MODE
(defun markdown-solarized ()
  (set-face-attribute 'markdown-header-face-1 nil
		      :height 1.3
		      :foreground "#bb3e06"
		      :extend t)
  (set-face-attribute 'markdown-header-face-2 nil
		      :height 1.2
		      :foreground "#778c00"
		      :extend t)
  (set-face-attribute 'markdown-header-face-3 nil
		      :height 1.15
		      :foreground "#007ec4"
		      :extend t)
  (set-face-attribute 'markdown-header-face-4 nil
		      :height 1.1
		      :foreground "#a67c00"
		      :extend t)
  (set-face-attribute 'markdown-header-face-5 nil
		      :foreground "#11948b"
		      :extend t)
  (set-face-attribute 'markdown-header-face-6 nil
		      :foreground "#778c00"
		      :extend t))

(use-package markdown-mode
  :ensure t
  :init
  (add-hook 'markdown-mode-hook
	    (lambda ()
	      (setq markdown-header-scaling t)
	      (when (equal current-theme 'solarized)
		(markdown-solarized))
	      (local-set-key (kbd "C-c SPC") 'counsel-outline))))

;;; LEDGER-MODE
(unless (package-installed-p 'ledger-mode)
  (package-install 'ledger-mode))
;; Load ledger-mode for '.dat' files
(add-to-list 'auto-mode-alist '("\\.dat\\'" . ledger-mode))

;;; CC-MODE
(require 'cc-mode)

;; LLVM format settings
(defun llvm-lineup-statement (langelem)
  (let ((in-assign (c-lineup-assignments langelem)))
    (if (not in-assign)
        '++
      (aset in-assign 0
            (+ (aref in-assign 0)
               (* 2 c-basic-offset)))
      in-assign)))

;; Add a cc-mode style for editing LLVM C and C++ code
(c-add-style "llvm.org"
             '("gnu"
	       (fill-column . 80)
	       (c++-indent-level . 2)
	       (c-basic-offset . 4)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist . ((arglist-intro . ++)
				   (innamespace . 0)
				   (member-init-intro . ++)
				   (statement-cont . llvm-lineup-statement)))))

(use-package eglot
  :ensure t)

(use-package color-identifiers-mode
  :ensure t)

;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))

(add-hook 'c-mode-hook
	  (lambda ()
	    (c-set-style "llvm.org")
	    ;; (local-set-key (kbd "<C-M-tab>") 'clang-format-buffer)
	    (local-set-key (kbd "<C-tab>") 'counsel-company)
	    (company-mode)
	    ;; (eglot-ensure)
	    (color-identifiers-mode t)))
	  
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))

;; ;;; XCSCOPE - cscope interface
;; (unless (package-installed-p 'xcscope)
;;   (package-install 'xcscope))
;; (require 'xcscope)
;; (cscope-setup)

;;; SCHEME DEVELOPMENT: Geiser package
(unless (package-installed-p 'geiser)
  (package-install 'geiser))
;; Set racket path
(setq geiser-racket-binary "/Applications/Racket v7.6/bin/racket")
;; Use Racket version
(setq geiser-active-implementations '(racket))
;; Showing matching parantheses
(show-paren-mode 1)

;;; RSS reader
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
	"https://www.uio.no/studier/emner/matnat/ifi/IN5050/v21/beskjeder/?vrtx=feed"
	"https://www.uio.no/studier/emner/matnat/ifi/IN5050/v21/slides/?vrtx=feed"
	;; Emacs stuff
	"http://pragmaticemacs.com/feed/"))

;;; JAVA DEVELOPMENT
(add-hook 'java-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (setq tab-width 4)
	    (set-fill-column 100)
	    (electric-pair-mode 1)))

;; ;;; Assembler DEVELOPMENT: gas-mode
;; ;; (require 'gas-mode)
;; ;; (add-to-list 'auto-mode-alist '("\\.S\\'" . gas-mode))
;; ;; Nasm-mode
;; ;; (unless (package-installed-p 'nasm-mode)
;; ;;   (package-install 'nasm-mode))
;; ;; Asm86 mode
;; ;; (autoload 'asm86-mode "~/.emacs/non-elpa/emacs86/asm86-mode.elc")
;; ;; ;; Make Emacs load Asm86 mode for .asm files.
;; ;; (setq auto-mode-alist
;; ;;       (append '(("\\.asm\\'" . asm86-mode) ("\\.inc\\'" . asm86-mode))
;; ;; 	      auto-mode-alist))
;; ;; ;; Enabling syntax highlighting.
;; ;; (add-hook 'asm86-mode 'turn-on-font-lock)
;; ;; "Recommended" color scheme.
;; ;; (cond ((fboundp 'global-font-lock-mode)
;; ;;        ;; Customize face attributes
;; ;;        (setq font-lock-face-attributes
;; ;;              ;; Symbol-for-Face Foreground Background Bold Italic Underline
;; ;;              '((font-lock-comment-face       "DarkGreen")
;; ;;                (font-lock-string-face        "Sienna")
;; ;;                (font-lock-keyword-face       "RoyalBlue")
;; ;;                (font-lock-function-name-face "Red")
;; ;;                (font-lock-variable-name-face "Black")
;; ;;                (font-lock-type-face          "Blue")
;; ;;                (font-lock-constant-face      "Purple")
;; ;;                ))
;; ;;        ;; Load the font-lock package.
;; ;;        (require 'font-lock)))
;; ;; (defun my-asm-mode-hook ()
;; ;;   ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
;; ;;   (local-unset-key (vector asm-comment-char))
;; ;;   ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
;; ;;   (setq tab-always-indent (default-value 'tab-always-indent)))
;; ;; (add-hook 'asm-mode-hook #'my-asm-mode-hook)


;; ;;; Frame size editor
;; ;; Function to set frame to half screen
;; ;; (defun halfscreen-frame ()
;; ;;   (interactive)
;; ;;   (set-frame-size nil (- (/ (display-pixel-width) 2) 20)
;; ;; 		  (display-pixel-height) t)
;; ;;   (revert-buffer nil 1))

;;; Ebuku - bookmark manager
(unless (package-installed-p 'ebuku)
  (package-install 'ebuku))
(require 'ebuku)

;;; rust-mode
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
	    (local-set-key (kbd "<return>") 'ignore)
	    (local-set-key (kbd "<backspace>") 'ignore)
	    (local-set-key (kbd "C-S-d") 'backward-delete-char-untabify)
	    (setq indent-tabs-mode nil)
	    (company-mode)
	    (racer-mode)))
(define-key rust-mode-map (kbd "C-c C-c") #'rust-check)
(define-key rust-mode-map (kbd "C-c C-r") #'rust-run)
(define-key rust-mode-map (kbd "C-c C-b") #'rust-compile)
(define-key rust-mode-map (kbd "C-c C-t") #'rust-test)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(add-to-list 'auto-mode-alist '("\\.lalrpop\\'" . rust-mode))
(setq company-tooltip-align-annotations t)
(setq company-tooltip-idle-delay 0.3)
(when (string= system-type "gnu/linux")
  (setq racer-rust-src-path
	"/home/danra/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/"))
;; LALRPOP files
(add-to-list 'auto-mode-alist '("\\.lalrpop\\'" . rust-mode))
;;; terminal-here
(setq terminal-here-linux-terminal-command 'xfce4-terminal)
;;; vterm
(use-package vterm
  :ensure t
  :commands vterm
  :config
  ;; To have enough buffer to look through output, but not so much that is negatively affects
  ;; performance.
  (setq vterm-max-scrollback 10000))


(use-package web-beautify
  :ensure t)

;;; html
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'web-mode
  '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

;;; TRAMP
(setq tramp-default-method "ssh")
;; ;;; clang-format
;; (use-package clang-format
;;   :ensure t
;;   :init
;;   (setq-default clang-format-style "file"))

;;; Eshell
(add-hook 'eshell-mode-hook 'company-mode)

;;; PDF
(use-package pdf-tools
  :ensure t
  :pin manual
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(add-hook 'pdf-view-mode-hook
	  (lambda ()
	    (linum-mode -1)
	    (display-line-numbers-mode 0)))

;; (server-start)

;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;       TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;       TeX-source-correlate-start-server t)

;; (add-hook 'TeX-after-compilation-finished-functions
;;           #'TeX-revert-document-buffer)
