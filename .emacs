;; Set the default theme (color format)
;;(setq custom-enabled-themes 'tango-dark)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
    'package-archives
    ;;'("melpa-stable" . "http://stable.melpa.org/packages/") t)
	'("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize))

(setq-default tab-width 2)

(setq load-path (cons (expand-file-name "~/") load-path))
(setq load-path (append load-path (list "~/emacs_stuff")))

(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))

(setq temporary-file-directory "~/emacs_stuff/tmp")

;; Add color-theme support
(add-to-list 'load-path "~/emacs_stuff/color-theme-6.6.0")
(add-to-list 'load-path "~/emacs_stuff/emacs-color-theme-solarized")
(require 'color-theme)
(require 'color-theme-solarized)
(require 'color-theme-tango)
(require 'color-theme-tangotango)
(require 'color-theme-tango-dark)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
	 ))

;; Standard BDI setup for emacs.  Please replace your home directory in the below.
;; Inform emacs where the standard build support elisp files are located.
;;(add-to-list 'load-path "~/bdi/rt/build/tools/emacs")
;; Require both Google and GN modes
(require 'google-c-style)
;; Set the default c-style to Google
(add-hook 'c-mode-common-hook 'google-set-c-style)
;; Turn on automatic indentation.
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
;; This function implements Boston Google style exceptions
;;(add-hook 'c-mode-common-hook 'boston-google-exceptions)

(load "~/emacs_stuff/clang-format.el")
;;(setq exec-path (append exec-path '("~/bdi/rt/build/tools/linux")))
(global-set-key [f11] 'clang-format-region)

(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)
(setq kill-whole-line t)
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set the "max" line width to 100
(setq-default fill-column 100)

;;(require 'bdi-cfg-mode)
;;(autoload 'bdi-cfg-mode "bdi-cfg" "Emacs mode for editing BD config files." t)
;;(add-to-list 'auto-mode-alist '("\\.cfg\\'" . bdi-cfg-mode))

(require 're-builder)
(setq reb-re-syntax 'string)

;; remove toolbar
(tool-bar-mode -1)

;; this allows limited auto complete with M-return, keep pressing to cycle
(global-set-key [M-return] 'dabbrev-expand)


;; Try using sourcepair instead of this.

;; Switches between source/header files
(defvar c++-source-extension-list '("c" "cc" "C" "cpp" "c++" "ipp"))
(defvar c++-header-extension-list '("h" "hh" "H" "hpp"))
;; Default extension for c++ header files.
(defvar c++-default-header-ext "h")
;; Default extension for c++ source files.
(defvar c++-default-source-ext "cpp")
;; Default regexp for c++ header files.
(defvar c++-header-ext-regexp "\\.\\(hpp\\|h\\|\hh\\|H\\)$")
;; Default regexp for c++ source files.
(defvar c++-source-ext-regexp "\\.\\(cpp\\|ipp\\|c\\|\cc\\|C\\)$")
(defvar c++-ext-regexp "\\.\\(hpp\\|h\\|\hh\\|H\\|cpp\\|ipp\\|c\\|\cc\\|C\\)$")

(defun toggle-source-header()
  "Switches to the source buffer if currently in the header buffer and vice versa."
  (interactive)
  (let ((buf (current-buffer))
	(name (file-name-nondirectory (buffer-file-name)))
	file
	offs)
    (setq offs (string-match c++-header-ext-regexp name))
    (if offs
	(let ((lst c++-source-extension-list)
	      (ok nil)
	      ext)
	  (setq file (substring name 0 offs))

	  (while (and lst (not ok))
	    (setq ext (car lst))
	    (if (file-exists-p (concat file "." ext))
								(setq ok t))
	    (setq lst (cdr lst)))
	  (if ok
	      (find-file (concat file "." ext))))
      (let ()
	(setq offs (string-match c++-source-ext-regexp name))
	(if offs
	    (let ((lst c++-header-extension-list)
		  (ok nil)
		  ext)
	      (setq file (substring name 0 offs))
	      (while (and lst (not ok))
		(setq ext (car lst))
		(if (file-exists-p (concat file "." ext))
		    (setq ok t))
		(setq lst (cdr lst)))
	      (if ok
		  (find-file (concat file "." ext)))))))))


(global-set-key [f7] 'toggle-source-header)

;; define shift based controls for switching and sizing windows
(global-set-key [(shift left)] 'windmove-left)
(global-set-key [(shift right)] 'windmove-right)
(global-set-key [(shift up)] 'windmove-up)
(global-set-key [(shift down)] 'windmove-down)

;; This allows the user to resize the region
(global-set-key [(shift control left)] 'shrink-window-horizontally)
(global-set-key [(shift control right)] 'enlarge-window-horizontally)
(global-set-key [(shift control up)] 'shrink-window)
(global-set-key [(shift control down)] 'enlarge-window)


;; Use etags file
;;(setq tags-table-list (list (getenv "BDI")))

(global-set-key (kbd "C-:") 'goto-line)

(setq auto-mode-alist
       (append
        (list
     (cons "\\.rc$" 'makefile-mode)
     (cons "\\akefile" 'makefile-mode)
     (cons "\\.h" 'c++-mode)
     (cons "\\.sf" 'makefile-mode)
     (cons "\\.jenkinsfile" 'groovy-mode)
     (cons "\\.groovy" 'groovy-mode))
        auto-mode-alist))

(setq-default inhibit-eol-conversion t)

;; Add the full path to the file in the current buffer to the title bar
;; Good for differentiating between local and server files
(setq frame-title-format '(:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))

(require 'recentf)

;; (require 'uniquify)
;; (setq uniquify-buffer-name-style 'forward)

(recentf-mode 1)

(global-set-key "\C-xf" 'recentf-open-files)

;; ;; Setup ggtags support:
;; (require 'ggtags)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;               (ggtags-mode 1))))

;; (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
;; (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
;; (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
;; (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
;; (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
;; (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

;; (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
;; ;; End ggtags support setup


;; Turn off the version control integration.  Helps load times
(setq vc-handled-backends nil)

;; Turn on line & column numbering
(setq line-number-mode t)
(setq column-number-mode t)

;; Set up some scrolling stuff
(setq scroll-step 1)
;;(setq scroll-conservatively 10000)


;; Make the compilation buffer scroll with output
;; 'first-error' stops at the first error
;; 'nil' doesn't scroll
;; anything else scrolls until the end.
(setq compilation-scroll-output t)
;;(setq compile-command "pushd $BDI; time build qnx; popd")

(global-set-key [f5] 'recompile)


;; Set up some stuff for using/editing Matlab code in emacs
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list
 'auto-mode-alist
 '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command "/Applications/MATLAB_R2018a.app/bin/matlab")
(setq matlab-shell-command-switches (list "-nodesktop"))


;; Add some functions for adjusting the current buffer width to a fixed size. One function accepts
;; an arbitrary buffer width, and a different function calls the first function, setting the width
;; to 100 characters.
(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-100-columns ()
  "Set the selected window to 100 columns."
  (interactive)
  (set-window-width fill-column))

(global-set-key "\C-x~" 'set-100-columns)
;;--------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(package-selected-packages (quote (matlab-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
