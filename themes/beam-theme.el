
;; ========================================================================
;; $File: beam-theme.el$
;; $Date: 2024-04-15$
;; $Revision: 1$
;; $Creator: Behiri$
;; $Notice: (C) Copyright 2024 by Behiri! All Rights Reserved.$
;; ========================================================================

(deftheme beam
  "Dark green theme with light text colors")
(let ((beam-font         "Liberation Mono") ;; DejaVu Sans Mono
      (beam-bg           "#001E13") ;; org dark green #040c08
      (beam-fg           "#f3e5c7") ;; #fff176 mine ;; #E0FFFF like it ;; #FFFACD try it ;; just white #FFFFE0
      (beam-bg-lighter   "#003E23") 
      (beam-modeline-fg  "#33ff33")
      (beam-highlights   "#113322")
      (beam-strings      "#FFBB33") ;; org #FFB000
      (beam-functions    "#30ccff") ;; org #16d3f2
      (beam-keywords     "#f3e5c7") ;; org #ffcc00
      (beam-comments     "#30FF60") ;; orginal color #439216
      (beam-black        "#000000")
      (beam-grey         "#888888")
      (beam-dark-green   "#218c23")
      (beam-green        "#0ecc11")
      (beam-light-green  "#8dcc78")
      (beam-bright-green "#46fc32")
      (beam-yellow       "#e6fc20")
      (beam-red          "#cc3333")
      (beam-dark-red     "#331111")
      (beam-hl-green     "#00250A")
      )

  (custom-theme-set-faces
   `beam
   `(default ((t (:background, beam-bg :foreground, beam-fg))))
   `(cursor  ((t (:background, beam-red :weight bold))))
   `(hl-line ((t (:background, beam-bg-lighter :weight normal))))
   `(mode-line ((t (:box nil, :background, beam-bg-lighter  :foreground, beam-modeline-fg) )))
   `(mode-line-inactive ((t (:inherit mode-line :background, beam-bg-lighter :foreground, beam-grey :box nil))))
   `(region ((t (:background, "#133343"))))
   `(minibuffer-prompt ((t (:foreground, beam-green))))
   '(fixed-pitch ((t (:family beam-font))))
   '(variable-pitch ((t (:family beam-font))))
   `(escape-glyph ((t (:foreground, beam-dark-green))))
   `(font-lock-builtin-face ((t (:foreground, beam-light-green))))
   `(font-lock-constant-face ((t (:inherit default))))
   `(font-lock-comment-face ((t (:foreground, beam-comments))))
   `(font-lock-string-face ((t (:foreground, beam-strings))))
   `(font-lock-preprocessor-face ((t (:foreground, beam-strings))))
   `(font-lock-keyword-face ((t (:foreground, beam-keywords)))) ;;  :inherit bold
   `(font-lock-function-name-face ((t (:foreground, beam-functions))))
   `(font-lock-type-face ((t (:inherit default))))
   `(font-lock-doc-face ((t (:foreground, beam-grey))))
   `(font-lock-variable-name-face ((t (:inherit default))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit bold))))
   `(font-lock-regexp-grouping-construct ((t (:inherit bold))))
   `(font-lock-highlighting-faces ((t (:foreground, beam-light-green))))
   `(font-lock-negation-char-face ((t (:foreground, beam-red :weight bold))))
   `(font-lock-warning-face ((t (:foreground, beam-yellow))))
   ;;`(highlight ((t (:background, "#133323" :foreground, "#BF5" :weight normal :extend))))
   `(highlight ((t (:extend t :background "#133323" :weight normal)))) ;; :foreground "#8C5" 
   `(lazy-highlight ((t (:extend t :background "#133323" :weight normal)))) ;; :foreground "#8C5" 
   ;; `(lazy-highlight ((t (:background, beam-highlights :foreground, beam-light-green))))
   `(tooltip ((t (:background, beam-grey :foreground, beam-black :inherit bold))))
   `(match ((t (:foreground, beam-grey :brackground, beam-black))))
   `(shadown ((t (:foreground, beam-dark-green))))
   `(secondary-selection ((t (:background, beam-dark-green :foreground, beam-grey))))
   `(link ((t (:foreground, beam-bright-green :underline, beam-bright-green :inherit bold))))
   `(link-visited ((t (:foreground, beam-green :underline, beam-green :inherit bold))))
   `(diff-added ((t (:background, beam-hl-green :foreground, beam-fg))))
   `(diff-changed ((t (:background, beam-bg-lighter :foreground, beam-bright-green))))
   `(diff-removed ((t (:background, beam-dark-red :foreground, beam-grey))))
   `(diff-context ((t (:inherit diff-changed))))
   `(diff-file-header ((t (:inherit diff-added))))
   `(diff-function ((t (:inherit diff-added))))
   `(diff-header ((t (:inherit diff-added))))
   `(diff-hunk-header ((t (:inherit diff-added))))
   `(diff-index ((t (:inherit diff-added))))
   `(diff-indicator-added ((t (:inherit diff-added))))
   `(diff-indicator-changed ((t (:inherit diff-changed))))
   `(diff-indicator-removed ((t (:inherit diff-removed))))
   `(diff-nonexistent ((t (:background, beam-black :foreground, beam-yellow))))
   `(diff-refine-added ((t (:inherit diff-added))))
   `(diff-refine-changed ((t (:inherit diff-changed))))
   `(diff-refine-removed ((t (:inherit diff-removed))))
   `(tool-bar ((t (:background, beam-black :foreground, beam-dark-green))))
   `(menu ((t (:background, beam-green :foreground, beam-black :box nil))))
   `(visible-bell ((t (:foreground, beam-black :background, beam-bright-green))))
   '(isearch ((t (:inherit defualt :background "#1F2F3F"))))
   `(search-fail ((t (:foreground, beam-red :background, beam-black))))

   `(whitespace-empty ((t (:background nil :foreground , beam-light-green))))
   `(whitespace-indentation ((t (:background nil :foreground , beam-light-green))))
   `(pace-line ((t (:background nil :foreground , beam-light-green))))
   `(whitespace-newline ((t (:background nil :foreground , beam-light-green))))
   `(whitespace-space ((t (:background nil :foreground , beam-light-green))))
   `(whitespace-space-after-tab ((t (:background nil :foreground , beam-light-green))))
   `(whitespace-space-before-tab ((t (:background nil :foreground , beam-light-green))))
   `(whitespace-tab ((t (:background nil))))
   `(whitespace-trailing ((t (:background , beam-light-green :foreground , beam-dark-green))))


   `(show-paren-match ((t (:foreground, beam-green :weight bold))))
   `(show-paren-mismatch ((t (:foreground, beam-red :weight bold))))

   `(ivy-current-match ((t (:extend t :background , beam-bg-lighter :weight bold)))) ;; :foreground "#8C5" 
;;   '(ivy-current-match ((t (:inherit highlight))))
;;   '(ivy-current-match ((t (:background , beam-light-green :foreground , beam-dark-green))))
   '(ivy-minibuffer-match-face-2 ((t (:inherit isearch :weight bold))))
   
   `(vertical-border ((t (:foreground, beam-bg-lighter))))
   `(info ((t (:foreground, beam-bright-green))))
   `(warning ((t (:foreground, beam-yellow))))
   `(error ((t (:foreground, beam-red))))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'beam)
