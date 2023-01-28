;;; USE-PACKAGE: isolate package configuration
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)


;;; HELM: incremental completion and narrowing selections
(use-package helm
  :ensure t
  :init (helm-mode 1))

(setq split-height-threshold nil)
(setq split-width-threshold 160)


;;; HELM-DESCBINDS: helm interface to describe-bindings
(use-package helm-descbinds
  :ensure t
  :init (helm-descbinds-mode))

;;; ACE-WINDOW: jump easier between windows
(use-package ace-window
  :ensure t
  :init
  (ace-window-display-mode)
  (setq aw-keys '(?a ?s ?d ?f)))


;;; PDF-TOOL: reading PDFs in Emacs
(use-package pdf-tools
  :ensure t
  :init
  (setq pdf-view-use-unicode-ligther nil)
  (setq-default pdf-view-display-size 'fit-width)
  :config
  (pdf-tools-install))

(defun db/pdf-view-mode-hook ()
  (linum-mode -1)
  (display-line-numbers-mode 0))

(add-hook 'pdf-view-mode-hook #'db/pdf-view-mode-hook)

;;; VISUAL FILL COLUMN: Center coloumns
(use-package visual-fill-column
  :ensure t)

(setq visual-fill-column-enable-sensible-window-split t)

(defun db/visual-fill-column-mode-hook ()
  (setq visual-fill-column-width 200) ; Should be condional due to monitor sizes
  (setq visual-fill-column-center-text t))

(add-hook 'visual-fill-column-mode-hook #'db/visual-fill-column-mode-hook)

;;; AUCTEX: Latex editing environment 
(use-package auctex
  :defer t
  :ensure t
  :init
  (when (string-equal system-type "darwin")
    (add-to-list 'exec-path "/opt/texlive/2021/bin/x86_64-linux"))
  (setq TeX-parse-self t))

(defun db/TeX-mode-hook ()
  ;; (flyspell-mode t)
  (auto-fill-mode t)
  (set-fill-column 100)
  (olivetti-mode t))

(add-hook 'TeX-mode-hook #'db/TeX-mode-hook)
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;       TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;       TeX-source-correlate-start-server t)

;; (add-hook 'TeX-after-compilation-finished-functions
;;           #'TeX-revert-document-buffer)


;;; YASNIPPET: template tool, pre-defined code snippets
(use-package yasnippet
  :ensure t
  :init
  (add-to-list 'load-path "~/Dropbox/yasnippets/"))


;;; COMPANY-MODE: COMPlete ANYthing, auto-completion framework
(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0.2
	company-async-timeout 15
	company-tooltip-align-annotations t)
  (setq company-backends '(company-capf
                           company-keywords
                           company-semantic
                           company-files
                           company-etags
                           company-clang
                           ;; company-irony-c-headers
                           ;; company-irony
                           ;; company-jedi
                           ;; company-ispell
                           ;; company-yasnippet
                           company-cmake)))


;;; magit: Git porcelain
(use-package magit
  :ensure t)


;;; Ivy: minibuffer completion
;; (use-package ivy
;;   :ensure t
;;   :init
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-count-format "(%d/%d) ")
;;   (setq ivy-height 30))


;;; counsel: Ivy-enhanced versions of common Emacs commands
;; (use-package counsel
;;   :ensure t
;;   :init
;;   (counsel-mode))


;;; Swiper: searching using Ivy
;; (use-package swiper
;;   :ensure t)



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


;;; ORG-MODE
(setq org-log-done t)

;; Disable actual width displays of images
(setq org-image-actual-width nil)

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

;; Path to capture files
(setq org-default-notes-file "~/Dropbox/org/capture/notes.org")

;; Fix indentation
(setq org-adapt-indentation t)

;; Make Org commands work on regions
(setq org-loop-over-headlines-in-active-region 'start-level)

;; Setting the time in mode line clock to be accumulated time on the current day
(setq org-clock-mode-line-total 'today)

(defun create-org-clock-heading ()
  (let ((last-two-headings (last (org-get-outline-path t) 2)))
    (format "%s/%s" (car last-two-headings) (cadr last-two-headings))))

(setq org-clock-heading-function #'create-org-clock-heading)

(setq org-clock-sound "~/.emacs.d/bells/dreamy.wav")

(setq org-agenda-prefix-format '((agenda . " %i %-20:c%?-12t% s")
				 (todo . " %i %-12:c")
				 (tags . " %i %-12:c")
				 (search . " %i %-12:c")))

(setq org-use-property-inheritance t)

(setq org-capture-templates
      '(("n" "Short note" entry
	(file+olp+datetree "~/Dropbox/org/capture/notes.org")
	 "* %a\n\n  %?"
	:emtpy-lines 1)))

;; Org bullets - beautify bullets in org-mode
(use-package org-bullets
  :ensure t)

(use-package org-tree-slide
  :ensure t)

(use-package org-tree-slide-pauses
  :ensure t
  :init (require 'org-tree-slide-pauses))

(defun db/org-mode-hook ()
  (org-bullets-mode 1)
  (outline-minor-mode t)
  (outline-hide-sublevels 1)
  (set-fill-column 95)
  (auto-fill-mode)
  (olivetti-mode)
  (setq olivetti-body-width 115))

(add-hook 'org-mode-hook #'db/org-mode-hook)

(add-hook 'org-tree-slide-play-hook
	  (lambda ()
	    ;; Display images inline
	    (org-display-inline-images) ;; Can also use org-startup-with-inline-images

	    ;; Scale the text.  The next line is for basic scaling:
	    (setq text-scale-mode-amount 3)
	    (text-scale-mode 1)))

	    ;; This option is more advanced, allows you to scale other faces too
	    ;; (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
	    ;; 				       (org-verbatim (:height 1.75) org-verbatim)
	    ;; 				       (org-block (:height 1.25) org-block)))))

(add-hook 'org-tree-slide-stop-hook
	  (lambda ()
	    (setq-local face-remapping-alist '((default variable-pitch default)))))

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


(use-package lsp-mode
  :ensure t)

;;; Eglot - lightweight LSP alternative
(use-package eglot
  :ensure t)

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
	       (fill-column . 120)
	       (c++-indent-level . 2)
	       (c-basic-offset . 2)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist . ((arglist-intro . ++)
				   (innamespace . 0)
				   (member-init-intro . ++)
				   (statement-cont . llvm-lineup-statement)))))

;; Color highlighting in CUDA files
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))

;; (use-package ggtags
;;   :ensure t)

(use-package company-c-headers
  :ensure t
  :init
  (add-to-list 'company-backends 'company-c-headers))

(defun db/c++-mode-hook ()
  (c-set-style "llvm.org")
  (color-identifiers-mode t)
  (company-mode)
  (visual-fill-column-mode)
  (eglot-ensure))

(defun db/c-mode-hook ()
  (c-set-style "k&r")
  (color-identifiers-mode t)
  (company-mode))
  ;; (eglot-ensure))

(add-hook 'c-mode-hook #'db/c-mode-hook)
(add-hook 'c++-mode-hook #'db/c++-mode-hook)

(use-package clang-format
  :ensure t
  :init
  (setq clang-format-executable "clang-format"))

;; Scheme mode
(setq scheme-program-name "plt-r5rs")
(setq scheme-default-implementation "plt-r5rs")
(add-hook 'scheme-mode-hook 'company-mode)

;;; geiser: Scheme development 
;; (use-package geiser
;;   :ensure t
;;   :init
;;   ;; Set racket path
;;   ;; (setq geiser-racket-binary "/Applications/Racket v7.6/bin/racket")
;;   (setq geiser-racket-binary "/usr/bin/racket")
;;   ;; Use Racket version
;;   (setq geiser-active-implementations '(racket)))

;; (use-package geiser-racket
;;   :ensure t)

;;; Elfeed - RSS reader
(use-package elfeed
  :ensure t
  :init
  (setq elfeed-feeds
	'(;; School stuff
	  ;; Emacs stuff
	  "http://pragmaticemacs.com/feed/"
	  "https://martinsteffen.github.io/feed.xml")))


(use-package lsp-java
  :ensure t)

(require 'lsp-java)

(setq lsp-headerline-breadcrumb-enable nil)

(defun db/java-mode-hook ()
  (setq indent-tabs-mode nil)		;insert spaces instead of tabs
  (setq tab-width 4)			;tab width = 4
  (set-fill-column 100)			;max line length = 100
  (electric-pair-mode 1)		;auto-pair symbols such as (), '', "", <>, etc.
  (setq lsp-headerline-breadcrumb-enable nil)
  (visual-fill-column-mode)
  (yas-minor-mode-on)
  (lsp))

;;; Java development
(add-hook 'java-mode-hook #'db/java-mode-hook)

;; Eclim
;; /home/danbanan/Documents/eclipse/eclipse/eclimd

(dap-register-debug-template "Siddhi app runner"
                             (list :type "java"
                                   :request "launch"
                                   :args "--burst-delay 1000 --burst-size 30 --count 30 -f /home/danbanan/dev/java/siddhi-instrumentation/java/modules/application/src/main/resources/events.txt"
                                   :vmArgs "-Xint -XX:+AlwaysPreTouch"
                                   :env '(("DEV" . "1"))))

;;; Ebuku - bookmark manager
(use-package ebuku
  :ensure t
  :init
  (require 'ebuku)
  (setq ebuku-results-limit 0))

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


;;; PYTHON DEVELOPMENT
;; (setq python-shell-remote-exec-path nil)
(setq python-shell-interpreter "/bin/python3")


;;; TERMINAL HERE - launch an external terminal
(use-package terminal-here ;; Might be interesting if exwm is installed
  :ensure t
  :init
  (setq terminal-here-linux-terminal-command 'gnome-terminal))

;;; VTERM
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
  (setq vterm-toggle-cd-auto-create-buffer t))

(defun db/vterm-toggle-cd ()
  (interactive)
  (setq vterm-buffer-name
	(concat "vterm-" (read-string "Vterm name:")))
  (vterm-toggle-cd))

;;; XML FORMAT - Easily reformat XML files
(use-package xml-format
  :ensure t
  :demand t
  :after nxml-mode)

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


;;; Projectile + Helm-projectile
(use-package helm-projectile
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (setq projectile-indexing-method 'alien)
  (setq projectile-project-search-path '(("~/Dropbox/" . 10) ("~/dev/" . 3)))
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile)
  (projectile-global-mode)
  (helm-projectile-on))

;;; Multiple Cursors
(use-package multiple-cursors
  :ensure t)


;; nXML
(setq nxml-child-indent 4)


;;; GNUPLOT
(use-package gnuplot
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode)))


;;; Shell scripting
(defun db/shell-hook ()
  (setq indent-tabs-mode t)
  (setq tab-width 4))

(add-hook 'sh-mode-hook #'db/shell-hook)


;;; EYEBROWSE: workspace manager
(use-package eyebrowse
  :ensure t
  :init (progn
	  (eyebrowse-mode t)
	  (setq eyebrowse-new-workspace t)))


;;; CPERL-MODE
;; Prefer cperl-mode to perl-mode, (more robust according to EmacsWiki)
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))
;; Also highlight referenced variables, not only when they are declared.
(setq cperl-highlight-variables-indiscriminately t)

(add-hook 'cperl-mode-hook #'db/perl-hook)

(defun db/perl-hook ()
  (cperl-set-style 'PerlStyle)
  (company-mode))
