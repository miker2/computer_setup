;; Set the default theme (color format)
;;(setq custom-enabled-themes 'tango-dark)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
	(package-initialize)
	)

(setq-default tab-width 2)

(setq load-path (cons (expand-file-name "~/") load-path))
(setq load-path (append load-path (list "~/emacs_stuff")))

(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))

(setq temporary-file-directory "~/emacs_stuff/tmp")

(add-to-list 'custom-theme-load-path "~/emacs_stuff/themes")

;;(setq load-path (cons (expand-file-name "/dir/with/cmake-mode") load-path))
;;(require 'cmake-mode)


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

;;(load "~/emacs_stuff/clang-format.el")
(require 'clang-format)
(setq exec-path (append exec-path '("/usr/local/bin")))
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
(menu-bar-mode -1)

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
 '(custom-enabled-themes '(tango-dark))
 '(custom-safe-themes
	 '("1ccdf932eb9a9107fbbf602d6c2f2d9412039b2417341c9d2181cd9c7efa94ab" "ac4d057902739eb8156af6d73be68482e7cd243a0639526cd71dbd764c9f5710" "aa2c4c7294bc47e1d2a75a5a848d2651a3cd5831f767e5072c6ce7d8ffd2c343" "04f944bf549505e84dc18e384e18a1687ec029594d935d6b6776bbc4961b1b58" "2024df7ae1ecd11a4646d073f85070b44915788e62a129c42c15e1bd65642620" "481c5decd002ddbb655fca645f882385f04106dae5ea096d42bb72137314e5b1" "301461eae09745ef1a380a9722e0b56d4a905261ab6b59584f7fd6718f0ca03a" "abdd8d875bc1f3853f2d904ae83c0cf47567615ca7deb71ca0122a5696292fde" "02fff7eedb18d38b8fd09a419c579570673840672da45b77fde401d8708dc6b5" "79afa0ec278155265cd12f7ac1bae801014f440d4e821d671ce2328b3ac40735" "4ed63f48b559488ed5628344bc3280ad212902d12c7f52f32362018419d46ab3" "0f544cad39494a86ebe31fc88bea4315484c58774d5f453d3ff2ae80054fdc6d" "bcd06819680dab167525050a2ec78f477bfdcdb3b2dd6c91a42517bcca39473d" "f52622b30f9a5e45f6331b9a5c57c92b7d2526fb43d374a115dca6aad17d683b" "5fb239be37cc7292db0b593ba6ae780b7f4e1c60323963d67a10612275da1630" "33ec826583ca3b05faa5bf04412f96e86e2b927721c106232e9e565717e718f2" "680f8b0aab16e080c250057f5b02130b1fe22098eaf834df0ad125cac7e47ceb" "2670afae1ad4a9addff20591ae5f2b4c925f244d9986604a3a56fc358f544def" "ea7bd29d749702b2be5a431299a4cb097a0f6abad17d38fa536d647a31ffa285" "1bb2c9a561b577e8129ea0b04cf3580ca074b0eabfa75dbc269c6bed0f4bedce" "42ced831b476d8326767c4a150cfeb3920bc24c01234cb6c42d0c90b7d800dc6" "5a1cea5ec0c56891010ab636e24e6ff1a65ee5083505cf65c7d39c4ecf43513d" "ca052516d4a60ad1ddbb4eaf931e599fc58c81938cf927333568f167205b596a" "f238808a8c86ab0f72c09949fd6dde230f881832d2e1dd27bf5b6e37fb44c9a5" "761d44dc06b3c8fff771435fd771b170d1bbdd71348b6aaaa6c0d0270d56cb70" "ac69b7e2e928dc1560d5a556043c99d8cb0614c30957bd03dfe82be4a9e917ee" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" "fa96a61e4eca5f339ad7f1f3442cb5a83696f6a45d9fe2a7bf3b75fc6912bb91" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" default))
 '(package-selected-packages
	 '(protobuf-mode modus-themes spacemacs-theme cyberpunk-theme tango-2-theme solarized-theme clang-format cmake-mode matlab-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
