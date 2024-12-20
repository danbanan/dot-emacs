;;; USE-PACKAGE: isolate package configuration
;; Load use-package explicitly if not already built-in
(unless (> 29 (string-to-number emacs-version))
  (progn 
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (require 'use-package)))

(customize-set-variable 'use-package-always-ensure t)
;; (customize-set-variable 'use-package-verbose t)

(use-package olivetti)

;;; HELM: incremental completion and narrowing selections
(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini)
   ("C-s" . helm-occur)
   :map helm-map
   ("C-h" . helm-mode-delete-char-backward-maybe)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action))
  :init
  (setq helm-show-completion-display-function #'helm-show-completion-default-display-function)
  (helm-mode 1))

(use-package helm-project
  :bind (("C-x C-p" . helm-project)
	 ([remap project-switch-to-buffer] . helm-project-buffers)
	 ([remap project-find-file] . helm-project-files)))

;; Helm interface for "complete anything" (company)
(use-package helm-company
  :after helm
  :after company
  :bind
  ((:map company-mode-map
	 ("C-:" . helm-company))
   (:map company-active-map
	 ("C-:" . helm-company))))

;;; HELM-DESCBINDS: helm interface to describe-bindings
(use-package helm-descbinds
  :after helm
  :config (helm-descbinds-mode))

;;; ACE-WINDOW: jump easier between windows
(use-package ace-window
  :custom (aw-keys '(?s ?d ?f ?g))
  :config (ace-window-display-mode))

;;; PDF-TOOL: reading PDFs in Emacs
(use-package pdf-tools
  :preface
  (defun db/pdf-tools-hook ()
    (display-line-numbers-mode 0))
  :init
  (pdf-loader-install t)
  :hook
  (pdf-tools-enabled . db/pdf-tools-hook)
  :bind
  (:map pdf-view-mode-map
	("C-s" . isearch-forward))
  :custom
  (pdf-view-use-unicode-ligther nil)
  (pdf-view-display-size 'fit-height))

;;; VISUAL FILL COLUMN: Center coloumns
(use-package visual-fill-column
  :custom
  (visual-fill-column-enable-sensible-window-split t)
  (visual-fill-column-width 200 "Should be conditional due to monitor sizes")
  (visual-fill-column-center-text t))

;;; AUCTEX: Latex editing environment
(use-package auctex
  :defer t
  :preface
  (defun db/TeX-mode-hook ()
    (auto-fill-mode t)
    (set-fill-column 100)
    (olivetti-mode t))
  :hook
  (TeX-mode-hook . db/TeX-mode-hook)
  (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
  :custom
  (TeX-parse-self t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  (TeX-source-correlate-start-server t))

;;; YASNIPPET: template tool, pre-defined code snippets
(use-package yasnippet
  :init
  (add-to-list 'load-path "~/Dropbox/yasnippets/"))

;;; COMPANY-MODE: COMPlete ANYthing, auto-completion framework
(use-package company
  :custom
  (company-idle-delay 0.1)
  (company-tooltip-align-annotations t))

;;; magit: Git porcelain
;; Ensures that magit-project-status is available in
;; project-switch-commands when loading project.el
(use-package magit
  :after project
  :config
  (require 'magit-extras))

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
(require 'org)

(setq org-log-done t)

;; Add markdown export
(eval-after-load "org"
  '(require 'ox-md nil t))

(setq org-latex-listings t)

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
	       '("ifimaster"
		 "\\documentclass{ifimaster}
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]
\\usepackage{babel,csquotes,ifimasterforside,url,varioref}"
		 ("\\chapter{%s}" . "\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; Disable actual width displays of images
(setq org-image-actual-width nil)

;; Path to agenda files
(setq-default org-agenda-files '("~/Dropbox/org/planner"))

;; Still show undone tasks in agenda even if deadline has passed
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

;; Clock report format in agenda
(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4))

;; Fontify bold, italics and underlined text without the pre-symbols
(setq org-hide-emphasis-markers nil)

(setq org-refile-targets '(("~/dev/common-lisp/timelisteserver/README.org" . (:maxlevel . 3))
			   ("~/dev/common-lisp/timelisteserver/BUGFIXES.org" . (:maxlevel . 3))))

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

(setq org-adapt-indentation t)

(setq org-capture-templates
      '(("w" "Work note with annotation" entry
	 (file+olp+datetree "~/Documents/capture/notes.org")
	 "* %?\n\n  [[%L][Capture point]]"
	 :tree-type month
	 :empty-lines 1)
	("W" "Work note without annotation" entry
	 (file+olp+datetree "~/Documents/capture/notes.org")
	 "* %?"
	 :tree-type month
	 :empty-lines 1)
	("t" "Todo with annotation" entry
	 (file "~/Documents/capture/notes.org")
	 "* TODO %?\n\n  [[%L][Capture point]]"
	 :tree-type month
	 :empty-lines 1)
	("T" "Todo without annotation" entry
	 (file "~/Documents/capture/notes.org")
	 "* TODO %?"
	 :tree-type month
	 :empty-lines 1)
	("p" "Personal note with annotation" entry
	 (file+olp+datetree "~/Dropbox/org/capture/notes.org")
	 "* [[%L][Capture point]]\n\n%?"
	 :empty-lines 1)
	("P" "Personal note without annotation" entry
	 (file+olp+datetree "~/Dropbox/org/capture/notes.org")
	 "* %?"
	 :empty-lines 1)
        ("a" "Affirmations" entry
         (file+olp+datetree "~/Dropbox/org/capture/affirmations.org")
         "* Three nice things about my workday\n\n  1) %?\n  2) \n  3) \n")))

;; Org bullets - beautify bullets in org-mode
(use-package org-bullets
  :after org-mode)

(use-package org-ref
  :after org-mode
  :config
  (setq org-latex-prefer-user-labels t)
  (require 'org-ref-helm))

;; (use-package helm-bibtex)

(setq bibtex-completion-bibliography '("~/Dropbox/emacs/bibliography/references.bib" "~/dev/latex/thesis/main/org/references.bib")
      bibtex-completion-library-path "~/Dropbox/emacs/bibliography/bibtex-pdfs/"
      bibtex-completion-notes-path "~/Dropbox/emacs/bibliography/notes/"
      bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
      bibtex-completion-additional-search-fields '(keywords)
      bibtex-completion-display-formats
      '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	(inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	(incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	(inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	(t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
      bibtex-completion-pdf-open-function #'find-file)

(defun db/org-mode-hook ()
  (org-bullets-mode 1)
  (outline-minor-mode t)
  (outline-hide-sublevels 1)
  (set-fill-column 95)
  (auto-fill-mode)
  (olivetti-mode)
  (setq olivetti-body-width 112))

(add-hook 'org-mode-hook #'db/org-mode-hook)

;;; Org-roam
;; (use-package org-roam
;;   :config
;;   (setq org-roam-directory "~/Documents/wiki/")
;;   (org-roam-db-autosync-mode))

;;; Calendar
;; (add-hook 'calendar-load-hook
;; 	  (calendar-set-date-style 'european))


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
  :custom
  (lsp-headerline-breadcrumb-enable nil))

;;; Color-indentifier - unique color per variable name
(use-package color-identifiers-mode)

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

(c-add-style "ExpressDrive"
	     '((c-basic-offset . 4)	; Guessed value
	       (c-offsets-alist
		(arglist-cont . 0)	    ; Guessed value
		(arglist-intro . +)	    ; Guessed value
		(block-close . 0)	    ; Guessed value
		(case-label . 0)	    ; Guessed value
		(defun-block-intro . +)	    ; Guessed value
		(defun-close . 0)	    ; Guessed value
		(defun-open . 0)	    ; Guessed value
		(statement . 0)		    ; Guessed value
		(statement-block-intro . +) ; Guessed value
		(statement-case-intro . +)  ; Guessed value
		(topmost-intro . 0)	    ; Guessed value
		(topmost-intro-cont . 0)    ; Guessed value
		(access-label . -)
		(annotation-top-cont . 0)
		(annotation-var-cont . +)
		(arglist-close . c-lineup-close-paren)
		(arglist-cont-nonempty . c-lineup-arglist)
		(block-open . 0)
		(brace-entry-open . 0)
		(brace-list-close . 0)
		(brace-list-entry . 0)
		(brace-list-intro . +)
		(brace-list-open . 0)
		(c . c-lineup-C-comments)
		(catch-clause . 0)
		(class-close . 0)
		(class-open . 0)
		(comment-intro . c-lineup-comment)
		(composition-close . 0)
		(composition-open . 0)
		(cpp-define-intro c-lineup-cpp-define +)
		(cpp-macro . -1000)
		(cpp-macro-cont . +)
		(do-while-closure . 0)
		(else-clause . 0)
		(extern-lang-close . 0)
		(extern-lang-open . 0)
		(friend . 0)
		(func-decl-cont . +)
		(inclass . +)
		(incomposition . +)
		(inexpr-class . +)
		(inexpr-statement . +)
		(inextern-lang . +)
		(inher-cont . c-lineup-multi-inher)
		(inher-intro . +)
		(inlambda . 0)
		(inline-close . 0)
		(inline-open . +)
		(inmodule . +)
		(innamespace . +)
		(knr-argdecl . 0)
		(knr-argdecl-intro . +)
		(label . 2)
		(lambda-intro-cont . +)
		(member-init-cont . c-lineup-multi-inher)
		(member-init-intro . +)
		(module-close . 0)
		(module-open . 0)
		(namespace-close . 0)
		(namespace-open . 0)
		(objc-method-args-cont . c-lineup-ObjC-method-args)
		(objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
		(objc-method-intro .
				   [0])
		(statement-case-open . 0)
		(statement-cont . +)
		(stream-op . c-lineup-streamop)
		(string . -1000)
		(substatement . +)
		(substatement-label . 2)
		(substatement-open . +)
		(template-args-cont c-lineup-template-args +))))

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

;;; GNU Global
(use-package ggtags
  :bind (:map ggtags-mode-map
	      ("C-c M-a" . ggtags-navigation-mode-abort)
	      ;; ("C-c g s" . ggtags-find-other-symbol)
	      ;; ("C-c g h" . ggtags-view-tag-history)
	      ;; ("C-c g r" . ggtags-find-reference)
	      ;; ("C-c g f" . ggtags-find-file)
	      ;; ("C-c g c" . ggtags-create-tags)
	      ;; ("C-c g u" . ggtags-update-tags)
	      ;; ("M-," . pop-tag-mark)
	      ))

(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers))

(defun db/c++-mode-hook ()
  (c-set-style "llvm.org")
  (color-identifiers-mode t)
  (company-mode)
  (visual-fill-column-mode)
  (eglot-ensure))

(defun db/c-mode-hook ()
  (c-set-style "EXPRESSDRIVE")
  (color-identifiers-mode t)
  (company-mode)
  (clang-format+-mode)
  (ggtags-mode)
  (olivetti-mode)
  (setq olivetti-body-width 85))

(add-hook 'c-mode-hook #'db/c-mode-hook)
(add-hook 'c++-mode-hook #'db/c++-mode-hook)

(use-package clang-format+
  :config
  (setq clang-format-executable "clang-format-16")
  (setq-default clang-format-style "file:/home/dolphin-dan/dev/c/.clang-format"))

;; Scheme mode
(setq scheme-program-name "plt-r5rs")
(setq scheme-default-implementation "plt-r5rs")
(add-hook 'scheme-mode-hook 'company-mode)

;;; geiser: Scheme development 
;; (use-package geiser
;;   
;;   :init
;;   ;; Set racket path
;;   ;; (setq geiser-racket-binary "/Applications/Racket v7.6/bin/racket")
;;   (setq geiser-racket-binary "/usr/bin/racket")
;;   ;; Use Racket version
;;   (setq geiser-active-implementations '(racket)))

;; (use-package geiser-racket
;;   )

;;; Elfeed - RSS reader
(use-package elfeed
  :init
  (setq elfeed-feeds
	'(;; School stuff
	  ;; Emacs stuff
	  "http://pragmaticemacs.com/feed/"
	  "https://martinsteffen.github.io/feed.xml")))


(use-package lsp-java
  :custom ;; Fix from https://github.com/emacs-lsp/lsp-java/issues/26#issuecomment-698573923
  (lsp-java-vmargs '("-Xmx1G"
		     "-XX:+UseG1GC"
		     "-XX:+UseStringDeduplication"
		     "-javaagent:/home/dan/.m2/repository/org/projectlombok/lombok/1.18.34/lombok-1.18.34.jar"))
  (lsp-java-format-settings-url "~/.emacs.d/resources/eclipse-java-google-style.xml"))

(defun db/java-mode-hook ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace 0 'local)
  (setq indent-tabs-mode nil)		;insert spaces instead of tabs
  (setq tab-width 4)			;tab width = 4
  (set-fill-column 100)			;max line length = 100
  (electric-pair-mode 1)		;auto-pair symbols such as (), '', "", <>, etc.
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
  :init
  (require 'ebuku)
  (setq ebuku-results-limit 0))

(load "ebuku")

;;; Rust development
(use-package rust-mode
  :init
  ;; Color encoding for LALRPOP files
  (add-to-list 'auto-mode-alist '("\\.lalrpop\\'" . rust-mode)))

;; Racer - completion, find definition, describe functions, and types in Rust
(use-package racer
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
  :init
  (setq terminal-here-linux-terminal-command 'gnome-terminal))

;;; VTERM
(use-package vterm
  :commands vterm
  :config
  ;; To have enough buffer to look through output, but not so much that is negatively affects
  ;; performance.
  (setq vterm-max-scrollback 10000))

(use-package vterm-toggle
  :init
  (setq vterm-toggle-cd-auto-create-buffer t))

(defun db/vterm-toggle-cd ()
  (interactive)
  (let ((custom-name (read-string "Vterm name:"))
	(machine-name (s-trim (shell-command-to-string "uname -n"))))
    (setq vterm-buffer-name
	  (concat "vterm-"
		  machine-name
		  (if (string-empty-p custom-name)
		      custom-name
		    (s-concat "-" custom-name))))
    (vterm-toggle-cd)))

;;; XML FORMAT - Easily reformat XML files
(use-package xml-format
  :demand t
  :after nxml-mode)

(use-package web-beautify)

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

;;; Eshell
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (company-mode)))

;;; Projectile + Helm-projectile
(use-package ppcompile
  :after projectile
  :bind
  (:map projectile-mode-map
	("C-x p c" . double-ppcompile))
  :config
  ;; TODO: Make more general
  (defun double-ppcompile (n)
    (interactive "cexd_cluster_(1|2|3|4)")
    (let* ((option (char-to-string n))
	   (cluster (pcase option
		      ("1" (cons "mxsw-10" "mxsw-11"))
		      ("2" (cons "sibiu-4" "sibiu-5"))
		      ("3" (cons "ni-transp-02" "mx-transp-01"))
		      ("4" (cons "rcy" "pxmxs-07"))
		      (_ nil)))
	   (launch-ppcompile (lambda (ssh-host)
			       (let ((bufname (format "*compilation-%s*" ssh-host)))
				 (when (get-buffer bufname)
				   (kill-buffer bufname))
				 (setq ppcompile-ssh-host ssh-host)
				 (ppcompile t)
				 (with-current-buffer "*compilation*"
				   (rename-buffer bufname))))))
      (when cluster
	(setq ppcompile-path-mapping-alist '(("/root/dan/ppcompile" . "/home/dolphin-dan/dev/c"))
	      ppcompile-rsync-dst-dir  "/root/dan/ppcompile"
	      ppcompile-ssh-user  "root"
	      ppcompile-remote-compile-command "cmake -B build -S . && make -C build module && make -C build/examples")
	(delete-other-windows)
	(funcall launch-ppcompile (car cluster))
	(funcall launch-ppcompile (cdr cluster))))))

(use-package helm-projectile
  :bind
  (("C-x p p" . helm-projectile-switch-project)
   :map projectile-mode-map
	("C-x p f" . helm-projectile-find-file)
	("C-x p d" . helm-projectile-find-dir)))

(use-package projectile
  :bind
  (:map projectile-mode-map
   ("C-x p s" . projectile-run-shell)
   ("C-x p b" . projectile-switch-to-buffer)
   ("C-x p v" . projectile-run-vterm)
   ("C-x p r" . dired-exd-sync))
  :custom
  (projectile-indexing-method 'alien)
  (projectile-completion-system 'helm)
  (projectile-switch-project-action 'helm-projectile)
  (projectile-find-dir-includes-top-level t)
  :config
  (projectile-mode)
  (helm-projectile-on))

(use-package dired-rsync)

(defun dired-exd-sync ()
  (interactive)
  (when (projectile-project-root)
    (find-file (projectile-project-root))
    (dired-toggle-marks)
    (message "before check")
    (cond ((string-match-p tramp-file-name-regexp default-directory)
	   (dired-rsync "/home/dolphin-dan/dev/c/expressdrive"))
	   (t (dired-rsync "/ssh:mx-transp-01:/root/dan/ppcompile/expressdrive")))))

;;; Multiple Cursors
(use-package multiple-cursors)

(require 'multiple-cursors)

;; nXML
(setq nxml-child-indent 4)

;;; GNUPLOT
(use-package gnuplot
  :init
  (add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode)))

;;; Shell scripting
(defun db/shell-hook ()
  (setq indent-tabs-mode nil)
  (setq sh-basic-offset 4))

(add-hook 'sh-mode-hook #'db/shell-hook)

;;; EYEBROWSE: workspace manager
(use-package eyebrowse  
  :init (progn
	  (eyebrowse-mode t)
	  (setq eyebrowse-new-workspace t)))

;;; PERL

;; (add-to-list 'load-path "~/.emacs.d/pde/")
;; (load "pde-load")

;; Prefer cperl-mode to perl-mode, (more robust according to EmacsWiki)
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))
;; Also highlight referenced variables, not only when they are declared.
(setq cperl-highlight-variables-indiscriminately t)

(defun db/perl-hook ()
  (cperl-set-style 'PBP)
  (company-mode)  
  (olivetti-mode)
  (setq-local olivetti-body-width 110)
  (cperl-set-style "PBP"))

(add-hook 'cperl-mode-hook #'db/perl-hook)

;; (setq Man-switches "")

;; (setenv "MANPATH" (concat "/home/dolphin-dan/perl5/man/man1:" (getenv "MANPATH")))
;; (setenv "MANPATH" (concat "/home/dolphin-dan/perl5/man/man3:" (getenv "MANPATH")))

(setenv "PERL5LIB" (concat "/home/dolphin-dan/perl5/lib/perl5/:" (getenv "PERL5LIB")))


;;; COMMON LISP
(add-to-list 'auto-mode-alist '("\\.cl\\'" . common-lisp-mode))
(setq inferior-lisp-program "/usr/local/bin/alisp")

;; SLY: Common Lisp IDE
(use-package sly
  :hook
  (sly-mrepl-mode . company-mode)
  (lisp-mode . sly-mode)
  ;; :custom
  ;; (inferior-lisp "alisp")
  :config
  (sly-setup)
  (setq sly-lisp-implementations
	'((alisp ("alisp"))
	  (mlisp ("mlisp"))
	  (cmucl-18d ("cmucl-18d"))
	  (cmucl-19d ("cmucl-19d"))
	  (cmucl-21d ("cmucl-21d"))
	  (sbcl ("sbcl")))))

;; (add-to-list 'sly-filename-translations
;;              (sly-create-filename-translator
;;               :machine-instance "Timelisteserver"
;;               :remote-host "172.16.7.206"
;;               :username "root"))

;;; Info
(add-to-list 'Info-directory-list (expand-file-name "~/.local/share/info/"))


;; R programming
(use-package ess
  :config
  (setq ess-use-ido nil))

;; Raku
;; (use-package raku-mode
;;   :config
;;   (require 'raku-skeletons)		; This is probably not the way to do it
;;   (auto-insert-mode)
;;   (define-auto-insert
;;     '("\\.rakumod\\'" . "Raku module skeleton")
;;     'raku-module-skeleton)
;;   (define-auto-insert
;;     '("\\.raku\\'" . "Raku script skeleton")
;;     'raku-script-skeleton))

;; Dolphin spesific
(use-package compile
  :ensure nil
  :config
  (add-to-list 'compilation-error-regexp-alist '("\\(minor: \\)\\(.*\\):\\([0-9]+\\)" 2 3 nil 1 nil (1 compilation-warning-face)))
  (add-to-list 'compilation-error-regexp-alist '("\\(critical: \\)\\(.*\\):\\([0-9]+\\)" 2 3 nil 2 nil (1 compilation-error-face))))


;; Set up work printer
(setq lpr-command "lp")
(setq lpr-switches
      (append '("-d" "EPSON_WF_4830_Series"
                "-o" "sides=two-sided-long-edge"
                "-o" "number-up=2")
              lpr-switches))
      
;; Docker
(use-package dockerfile-mode
  :ensure t)
