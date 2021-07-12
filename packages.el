;;; use-package - isolate package configuration
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


;;; Ace-window - jump easier between windows
(use-package ace-window
  :ensure t
  :init
  (ace-window-display-mode)
  (setq aw-keys '(?a ?s ?d ?f)))


;;; PDF
(use-package pdf-tools
  :ensure t
  :init
  (setq pdf-view-use-unicode-ligther nil)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width))

(add-hook 'pdf-view-mode-hook
	  (lambda ()
	    (linum-mode -1)
	    (display-line-numbers-mode 0)))


;;; Latex
(use-package auctex
  :ensure t
  :init
  (when (string-equal system-type "darwin")
    (add-to-list 'exec-path "/opt/texlive/2021/bin/x86_64-linux"))
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  :hook (flyspell-mode
	 LaTeX-math-mode))

;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;       TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;       TeX-source-correlate-start-server t)

;; (add-hook 'TeX-after-compilation-finished-functions
;;           #'TeX-revert-document-buffer)



;;; YASnippet - template tool for Emacs
(use-package yasnippet
  :ensure t
  :init
  (add-to-list 'load-path "~/Dropbox/yasnippets/"))


;;; Company - complete anything
(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0.3
	company-async-timeout 15
	company-tooltip-align-annotations t))


;;; Magit - Git porcelain
(use-package magit
  :ensure t)


;;; Ivy - Minibuffer completion
(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 30))


;;; Counsel - Ivy-enhanced versions of common Emacs commands
(use-package counsel
  :ensure t
  :init
  (counsel-mode))


;;; Swiper
(use-package swiper
  :ensure t)


;;; Lines Settings
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode t))

;; (global-linum-mode 1)

;; (setq linum-format " %d")


;; Set relative line numbers 
(setq display-line-numbers-type 'visual)

;; Display current column number
(setq column-number-mode t)

;; Adjust line spacing
(setq line-spacing 0.1)


;;; Lisp mode
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (outline-minor-mode t)
	    (company-mode)
	    (electric-pair-mode t)))


;;; Org mode
(setq org-log-done t)

;; Path to agenda files
(setq-default org-agenda-files '("~/Dropbox/org/planner"))

;; Still show undone tasks in agenda even if deadline has passed
(setq org-agenda-skip-scheduled-if-deadline-is-shown 'repeated-after-deadline)

;; Clock report format in agenda
(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4))

;; Fontify bold, italics and underlined text without the pre-symbols
(setq org-hide-emphasis-markers t)

;; Global Org TODO keywords
(setq org-todo-keywords '((sequence "TODO(t)"
				    "IN-PROGRESS(i)"
				    "WAITING(w)"
				    "|"
				    "CANCELLED(c)"
				    "POSTPONED(p)"
				    "DONE(d)")))

;; Make Org commands work on regions
(setq org-loop-over-headlines-in-active-region 'start-level)

;; Org bullets - beautify bullets in org-mode
(use-package org-bullets
  :ensure t)

(add-hook 'org-mode-hook
	  (lambda ()
	    (org-bullets-mode 1)
	    (outline-minor-mode t)
	    (outline-hide-sublevels 1)))


;;; Calender
(add-hook 'calendar-load-hook
	  (calendar-set-date-style 'european))


;;; Markdown-mode
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
		(markdown-solarized)))))


;;; LEDGER-MODE
(unless (package-installed-p 'ledger-mode)
  (package-install 'ledger-mode))
;; Load ledger-mode for '.dat' files
(add-to-list 'auto-mode-alist '("\\.dat\\'" . ledger-mode))


;;; Eglot - lightweight LSP alternative
;; (use-package eglot
;;   :ensure t
;;   :init
;;   (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd")))


;;; Color-indentifier - unique color per variable name
(use-package color-identifiers-mode
  :ensure t)


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

;; Color highlighting in CUDA files
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))

(use-package ggtags
  :ensure t)

(use-package company-c-headers
  :ensure t)

(add-hook 'c-mode-hook
	  (lambda ()
	    (c-set-style "llvm.org")
	    (ggtags-mode 1)
	    (company-mode)
	    (setq company-backends (remove 'company-clang company-backends))
	    (when (not (member 'company-c-headers company-backends))
	      (push 'company-c-headers company-backends))
	    ;; (eglot-ensure)
	    (color-identifiers-mode t)))
	  

;;; SCHEME DEVELOPMENT: Geiser package
(use-package geiser
  :ensure t
  :init
  ;; Set racket path
  (setq geiser-racket-binary "/Applications/Racket v7.6/bin/racket")
  ;; Use Racket version
  (setq geiser-active-implementations '(racket)))


;;; Elfeed - RSS reader
(use-package elfeed
  :ensure t
  :init
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
	  "http://pragmaticemacs.com/feed/")))


;;; Java development
(add-hook 'java-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (setq tab-width 4)
	    (set-fill-column 100)
	    (electric-pair-mode 1)))


;;; Ebuku - bookmark manager
(use-package ebuku
  :ensure t
  :init
  (require 'ebuku))


;;; Rust development
(use-package rust-mode
  :ensure t
  :init
  ;; Color encoding for LALRPOP files
  (add-to-list 'auto-mode-alist '("\\.lalrpop\\'" . rust-mode)))

;; Racer - completion, find definition, describe functions, and types in Rust
(use-package racer
  :ensure t
  :init
  (when (string= system-type "gnu/linux")
    (setq racer-rust-src-path
	  "/home/danra/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/")))

(add-hook 'rust-mode-hook
	  (lambda ()
	    ;; Spaces over tabs recommended by Rust style guide
	    (setq indent-tabs-mode nil)
	    (company-mode)
	    (racer-mode)))


;;; Python development
;; (setq python-shell-remote-exec-path nil)
(setq python-shell-interpreter "python3")

;;; Terminal-here - launch an extern terminal
;; Might be interesting if exwm is installed
;; (setq terminal-here-linux-terminal-command 'xfce4-terminal)


;;; Vterm
(use-package vterm
  :ensure t
  :commands vterm
  :config
  ;; To have enough buffer to look through output, but not so much that is negatively affects
  ;; performance.
  (setq vterm-max-scrollback 10000))

(use-package vterm-toggle
  :ensure t
  :init
  (setq vterm-buffer-name "*vterm*")
  (setq vterm-toggle-cd-auto-create-buffer t))


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
(add-hook 'eshell-mode-hook
	  (lambda ()
	    ;; eshell-mode-map is defined locally only
	    (define-key eshell-mode-map (kbd "C-j") 'eshell-send-input)
	    (company-mode)))


;;; Projectile
(use-package projectile
  :ensure t
  :init (projectile-mode))
