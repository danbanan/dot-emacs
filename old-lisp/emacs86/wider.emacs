;; Sample customization file for Emacs Asm86 major mode. The file
;; must be saved as ".emacs" to be recognized properly. 
;;
;; This example file disables electric colon and semicolon, and
;; changes the indentation so that it is more pronounced (wider)
;; than the default. Extended-style headers are used. 
;;
;; Use it as a starting point for your own customization. Be
;; sure to change the file path in the (autoload ) statement
;; to reflect where you actually saved the file asm86-mode.elc.
;;
;; by Gabe Wenz
;; wenz@ugcs.caltech.edu
;;

;;; MISC. CUSTOMIZATION ;;;

;; The first statement below is essential to avoid FS problems, because  
;; windows is not good at handling >3 character extensions. This may not
;; be a problem on later versions of Windows. 
(setq make-backup-files nil)         ; no more of those annoying ~ files
(setq auto-save-default nil)         ; turn off auto-saving
(global-set-key [?\M-g] 'goto-line)  ; easier to jump to specific lines (M-g)



;;; ASM86 MODE "INSTALLATION" ;;;

;; This command loads the .elc file that defines Asm86 mode.  Be sure
;; that the file path is correct (forward slashes, not back slashes,
;; separate directories), and make the file extension ".el" instead
;; of ".elc" if you downloaded the Lisp source version instead of the
;; byte-compiled version.
(autoload 'asm86-mode "~/linux/emacs/asm86-mode.elc")       ;loads "asm86-mode.elc"

;; Make Emacs load Asm86 mode for .asm files. This command 
;; does not need any modifications. 
(setq auto-mode-alist
     (append '(("\\.asm\\'" . asm86-mode) ("\\.inc\\'" . asm86-mode)) auto-mode-alist))


;;; Customization for syntax highlighting ;;;

;; Enable syntax highlighting for Asm86 Mode:
(add-hook 'asm86-mode-hook 'turn-on-font-lock)

;; Customize syntax colors. This is the "recommended" 
;; color scheme. 
(cond ((fboundp 'global-font-lock-mode)
       ;; Customize face attributes
       (setq font-lock-face-attributes
             ;; Symbol-for-Face Foreground Background Bold Italic Underline
             '((font-lock-comment-face       "DarkGreen")
               (font-lock-string-face        "Sienna")
               (font-lock-keyword-face       "RoyalBlue")
               (font-lock-function-name-face "Red")
               (font-lock-variable-name-face "Black")
               (font-lock-type-face          "Blue")
               (font-lock-constant-face      "Purple")
               ))
       ;; Load the font-lock package.
       (require 'font-lock)))


;;; CUSTOMIZE ASM86 MODE BEHAVIOR ;;;

; The function setq-default sets the global (not buffer local)
; value of the variable. 
(setq-default comment-column 50)     ; column where inline comments start


;; Set behavior of some specific features:
(setq asm86-electric-colon-on nil)     ; disable electric colon
(setq asm86-electric-semicolon-on nil) ; disable semielectric colon

(setq asm86-electric-gap-size 8)     ; wider than default (6)

(setq asm86-extended-style-headers t); use more complete function headers


;; Define indentation behavior (wider than default).
;; Most of the code is either a comment, instruction,
;; or a blank line, so these are the only lines where
;; the default is overridden. 
(setq asm86-blank-base-offset 5)
(setq asm86-header-comment-base-offset 5)
(setq asm86-code-comment-base-offset 5)
(setq asm86-inline-comment-base-offset 5)
(setq asm86-inst-base-offset 5)

; Within a function, indent an additional 5
; spaces (instead of the default 3):
(setq asm86-blank-func-offset 5)
(setq asm86-label-func-offset 5)
(setq asm86-header-comment-func-offset 5)
(setq asm86-code-comment-func-offset 5)
(setq asm86-inline-comment-func-offset 5)
(setq asm86-inst-func-offset 5)

