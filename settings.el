;;;* Basic setup
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (file-exists-p (expand-file-name
			 (concat package-user-dir
				 "/archives/MELPA/")))
  (package-refresh-contents))

;; Set font
(set-face-attribute 'default nil :family "Iosevka Aile Extralight" :height 150)
;; Disable menu bar
(menu-bar-mode -1)
;; Disable tool bar
(tool-bar-mode -1)
;; Disable scroll bar
(scroll-bar-mode -1)
;; Sort completions vertically
(setq completions-format 'vertical)

;;; Disable start-up screen
;;; ------------------------------------------------------------------
(setq inhibit-startup-message t)
;;; -----------------------------------------------------------------

;;;* File system setup
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

;;; Set default directory on start-up
; --------------------------------------------------------------------
(setq default-directory "~/Dropbox/")
; --------------------------------------------------------------------
