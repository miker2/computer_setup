;;; Color theme based on Tango Palette. Created by danranx@gmail.com
(defun color-theme-tango-dark ()
  "A color theme based on Tango-Dark custom theme from emacs24."

  (interactive)
  (let (
		;; Tango palette colors.
		(butter-1 "#fce94f") (butter-2 "#edd400") (butter-3 "#c4a000")
		(orange-1 "#fcaf3e") (orange-2 "#f57900") (orange-3 "#ce5c00")
		(choc-1 "#e9b96e") (choc-2 "#c17d11") (choc-3 "#8f5902")
		(cham-1 "#8ae234") (cham-2 "#73d216") (cham-3 "#4e9a06")
		(blue-1 "#729fcf") (blue-2 "#3465a4") (blue-3 "#204a87")
		(plum-1 "#ad7fa8") (plum-2 "#75507b") (plum-3 "#5c3566")
		(red-1 "#ef2929")  (red-2 "#cc0000")  (red-3 "#a40000")
		(alum-1 "#eeeeec") (alum-2 "#d3d7cf") (alum-3 "#babdb6")
		(alum-4 "#888a85") (alum-5 "#555753") (alum-6 "#2e3436")
		;; Not in Tango palette; used for better contrast.
		(cham-0 "#b4fa70") (blue-0 "#8cc4ff") (plum-0 "#e6a8df")
		(red-0 "#ff4b4b")  (alum-5.5 "#41423f") (alum-7 "#212526"))


  (color-theme-install
   `(color-theme-tango-dark
     ((background-color . ,alum-6)
      (background-mode . dark)
      (border-color . "#888a85")
      (foreground-color . ,alum-1)
      (cursor-color . ,butter-1)
      (mouse-color . "#8ae234"))
     ((help-highlight-face . underline)
      (ibuffer-dired-buffer-face . font-lock-function-name-face)
      (ibuffer-help-buffer-face . font-lock-comment-face)
      (ibuffer-hidden-buffer-face . font-lock-warning-face)
      (ibuffer-occur-match-face . font-lock-warning-face)
      (ibuffer-read-only-buffer-face . font-lock-type-face)
      (ibuffer-special-buffer-face . font-lock-keyword-face)
      (ibuffer-title-face . font-lock-type-face))
     (cursor ((t (:background ,butter-1 :foreground nil))))
	 ;; Highlighting faces
     (fringe ((t (:background ,alum-6))))
     (highlight ((t (:background ,butter-2 :foreground ,alum-6))))
     (region ((t (:background ,alum-5))))
     (secondary-selection ((t (:background ,blue-3))))
     (isearch ((t (:background ,orange-3 :foreground ,alum-1))))
     (lazy-highlight ((t (:background ,choc-3))))
	 (trailing-whitespace ((t (:background ,red-3))))
     (border ((t (:background "#888a85"))))
	 ;; Mode line faces
     (mode-line ((t (:foreground "#2e3436" :background "#d3d7cf"))))
     (mode-line-inactive ((t (:foreground "#bbbbbc" :background "#555753"))))
     ;;(mode-line-buffer-id ((t (:bold t :foreground "orange" :background nil))))
	 ;; Font lock faces
     (font-lock-builtin-face ((t (:foreground ,plum-1))))
     (font-lock-comment-face ((t (:foreground ,cham-2))))
     (font-lock-constant-face ((t (:foreground ,plum-0))))
     (font-lock-doc-face ((t (:foreground ,choc-1 :italic t))))
     (font-lock-keyword-face ((t (:foreground ,cham-0 :bold t))))
     (font-lock-string-face ((t (:foreground ,choc-1))))
     (font-lock-type-face ((t (:foreground ,blue-0 :bold t))))
     (font-lock-variable-name-face ((t (:foreground ,orange-1))))
     (font-lock-warning-face ((t (:bold t :foreground ,red-0))))
     (font-lock-function-name-face ((t (:foreground ,butter-1 :bold t :italic t))))
	 ;; 
     (comint-highlight-input ((t (:italic t :bold t))))
     (comint-highlight-prompt ((t (:foreground "#8ae234"))))
     (show-paren-match-face ((t (:foreground "#2e3436" :background "#73d216"))))
     (show-paren-mismatch-face ((t (:background "#ad7fa8" :foreground "#2e3436"))))
     (minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
     (info-xref ((t (:foreground "#729fcf"))))
     (info-xref-visited ((t (:foreground "#ad7fa8"))))
	 ;; Ediff faces
	 (ediff-current-diff-A ((t (:background ,alum-5))))
	 (ediff-fine-diff-A ((t (:background ,blue-3))))
	 (ediff-even-diff-A ((t (:background ,alum-5.5))))
	 (ediff-odd-diff-A ((t (:background ,alum-5.5))))
	 (ediff-current-diff-B ((t (:background ,alum-5))))
	 (ediff-fine-diff-B ((t (:background ,choc-3))))
	 (ediff-even-diff-B ((t (:background ,alum-5.5))))
	 (ediff-odd-diff-B ((t (:background ,alum-5.5))))
	 ;; Flyspell faces
	 (flyspell-duplicate ((t (:underline ,orange-1))))
	 (flyspell-incorrect ((t (:underline ,red-1))))
	 ;;;; Semantic faces
	 ;;(semantic-decoration-on-includes ((t (:underline ,alum-4))))
	 ;;(semantic-decoration-on-private-members-face
	 ;; ((t (:background ,plum-3))))
	 ;;(semantic-decoration-on-protected-members-face
	 ;; ((t (:background ,choc-3))))
	 ;;(semantic-decoration-on-unknown-includes
	 ;; ((t (:background ,red-3))))
	 ;;(semantic-decoration-on-unparsed-includes
	 ;; ((t (:background ,alum-5.5))))
	 ;;(semantic-tag-boundary-face ((t (:overline ,blue-1))))
	 ;;(semantic-unmatched-syntax-face ((t (:underline ,red-1))))

     ))))

(provide 'color-theme-tango-dark)
