;; -*- lexical-binding: t; -*-

;; ========================================================================
;; $File: beam-theme.el$
;; $Date: 2024-04-15$
;; $Revision: 1$
;; $Creator: Behiri$
;; $Notice: (C) Copyright 2024 by Behiri! All Rights Reserved.$
;; ========================================================================


;;;###theme-autoload
(deftheme beam
  "Dark green theme with light text colors."
  :background-mode 'dark
  :kind 'color-scheme
  :family 'beam)

(let* ((beam-font            "Liberation Mono")
       (beam-bg              "#001E13") ;; 022627
       (beam-fg              "#f3e5c7") ;; D3B48B
       (beam-bg-lighter      "#004E33") 
       (beam-modeline-fg     "#33ff33")
       (beam-strings         "#FFBB33") 
       (beam-functions       "#30ccff") 
       (beam-keywords        "#f3e5c7") 
       (beam-comments        "#30FF60") 
       (beam-black           "#000000")
       (beam-grey            "#888888")
       (beam-dark-green      "#218c23")
       (beam-green           "#0ecc11")
       (beam-light-green     "#8dcc78")
       (beam-bright-green    "#46fc32")
       (beam-yellow          "#e6fc20")
       (beam-red             "#cc3333")
       (beam-dark-red        "#331111")
       (beam-hl-green        "#00350A")
       (beam-hl-line         "#003428")
       (beam-highlight       "#633323")
       (beam-isearch         "#113355") ;; E97A9E
       (beam-region          "#1100ff")
       (beam-ivy-match       "#105E43")
       (beam-constant        "#105E43")
       

       ;; Face-specific colors
       (beam-default-bg                      beam-bg)
       (beam-default-fg                      beam-fg)
       (beam-cursor-bg                       beam-red)
       (beam-hl-line-bg                      beam-hl-line)
       (beam-mode-line-bg                    beam-bg-lighter)
       (beam-mode-line-fg                    beam-modeline-fg)
       (beam-mode-line-inactive-bg           beam-bg-lighter)
       (beam-mode-line-inactive-fg           beam-grey)
       (beam-region-bg                       beam-region)
       (beam-minibuffer-prompt-fg            beam-green)
       (beam-escape-glyph-fg                 beam-dark-green)
       (beam-font-lock-builtin-face-fg       beam-light-green)
       (beam-font-lock-comment-face-fg       beam-comments)
       (beam-font-lock-string-face-fg        beam-strings)
       (beam-font-lock-keyword-face-fg       beam-keywords)
       (beam-font-lock-function-name-face-fg beam-functions)
       (beam-font-lock-doc-face-fg           beam-grey)
       (beam-font-lock-negation-char-face-fg beam-red)
       (beam-font-lock-warning-face-fg       beam-yellow)
       (beam-highlight-bg                    beam-highlight)
       (beam-lazy-highlight-bg               beam-highlight)
       (beam-match-fg                        beam-grey)
       (beam-match-bg                        beam-black)
       (beam-shadow-fg                       beam-dark-green)
       (beam-secondary-selection-bg          beam-dark-green)
       (beam-secondary-selection-fg          beam-grey)
       (beam-tool-bar-bg                     beam-black)
       (beam-tool-bar-fg                     beam-dark-green)
       (beam-menu-bg                         beam-green)
       (beam-menu-fg                         beam-black)
       (beam-visible-bell-fg                 beam-black)
       (beam-visible-bell-bg                 beam-bright-green)
       (beam-isearch-bg                      beam-isearch)
       (beam-search-fail-fg                  beam-red)
       (beam-search-fail-bg                  beam-black)
       (beam-show-paren-match-fg             beam-green)
       (beam-show-paren-mismatch-fg          beam-red)
       (beam-ivy-current-match-bg            beam-ivy-match)
       (beam-vertical-border-fg              beam-bg-lighter)
       (beam-info-fg                         beam-bright-green)
       (beam-warning-fg                      beam-yellow)
       (beam-error-fg                        beam-red)

       ;; Diff faces
       (beam-diff-added-bg      beam-hl-green)
       (beam-diff-added-fg      beam-fg)
       (beam-diff-changed-bg    beam-bg-lighter)
       (beam-diff-changed-fg    beam-bright-green)
       (beam-diff-removed-bg    beam-dark-red)
       (beam-diff-removed-fg    beam-grey)
       (beam-diff-nonexistent-bg beam-black)
       (beam-diff-nonexistent-fg beam-yellow)

       ;; Whitespace faces
       (beam-whitespace-fg          beam-light-green)
       (beam-whitespace-trailing-bg beam-light-green)
       (beam-whitespace-trailing-fg beam-dark-green)

       ;; Links
       (beam-link-fg           beam-bright-green)
       (beam-link-visited-fg   beam-green)
       )
 
  (custom-theme-set-faces
   `beam
   ;; Basic settings
   `(default ((t (:background ,beam-default-bg :foreground ,beam-default-fg :height 113))))
   `(cursor  ((t (:background ,beam-cursor-bg :weight bold))))
   `(hl-line ((t (:background ,beam-hl-line-bg :weight normal))))
   `(mode-line ((t (:box nil :background ,beam-mode-line-bg :foreground ,beam-mode-line-fg))))
   `(mode-line-inactive ((t (:inherit mode-line :background ,beam-mode-line-inactive-bg :foreground ,beam-mode-line-inactive-fg :box nil))))
   `(region ((t (:background ,beam-region-bg))))
   `(minibuffer-prompt ((t (:foreground ,beam-minibuffer-prompt-fg))))
   `(fixed-pitch ((t (:family ,beam-font))))
   `(variable-pitch ((t (:family ,beam-font))))
   `(escape-glyph ((t (:foreground ,beam-escape-glyph-fg))))
   
   ;; Font lock faces
   `(font-lock-builtin-face ((t (:foreground ,beam-font-lock-builtin-face-fg))))
   `(font-lock-constant-face ((t (:inherit default))))
   `(font-lock-comment-face ((t (:foreground ,beam-font-lock-comment-face-fg))))
   `(font-lock-string-face ((t (:foreground ,beam-font-lock-string-face-fg))))
   `(font-lock-preprocessor-face ((t (:foreground ,beam-font-lock-string-face-fg))))
   `(font-lock-keyword-face ((t (:foreground ,beam-font-lock-keyword-face-fg :weight bold))))
   `(font-lock-function-name-face ((t (:foreground ,beam-font-lock-function-name-face-fg))))
   `(font-lock-type-face ((t (:inherit default))))
   `(font-lock-doc-face ((t (:foreground ,beam-font-lock-doc-face-fg))))
   `(font-lock-variable-name-face ((t (:inherit default))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit bold))))
   `(font-lock-regexp-grouping-construct ((t (:inherit bold))))
   `(font-lock-highlighting-faces ((t (:foreground ,beam-font-lock-builtin-face-fg))))
   `(font-lock-negation-char-face ((t (:foreground ,beam-font-lock-negation-char-face-fg :weight bold))))
   `(font-lock-warning-face ((t (:foreground ,beam-font-lock-warning-face-fg))))
   
   ;; Highlight and search
   `(highlight ((t (:extend t :background ,beam-highlight-bg :weight normal))))
   `(lazy-highlight ((t (:extend t :background ,beam-lazy-highlight-bg :weight normal))))
   `(match ((t (:foreground ,beam-match-fg :background ,beam-match-bg))))
   `(shadow ((t (:foreground ,beam-shadow-fg))))
   `(secondary-selection ((t (:background ,beam-secondary-selection-bg :foreground ,beam-secondary-selection-fg))))

   ;; UI elements
   `(tool-bar ((t (:background ,beam-tool-bar-bg :foreground ,beam-tool-bar-fg))))
   `(menu ((t (:background ,beam-menu-bg :foreground ,beam-menu-fg :box nil))))
   `(visible-bell ((t (:foreground ,beam-visible-bell-fg :background ,beam-visible-bell-bg))))
   `(isearch ((t (:inherit default :background ,beam-isearch-bg))))
   `(search-fail ((t (:foreground ,beam-search-fail-fg :background ,beam-search-fail-bg))))
   
   ;; Parentheses matching
   `(show-paren-match ((t (:foreground ,beam-show-paren-match-fg :weight bold))))
   `(show-paren-mismatch ((t (:foreground ,beam-show-paren-mismatch-fg :weight bold))))
   
   ;; Ivy faces
   `(ivy-current-match ((t (:extend t :background ,beam-ivy-current-match-bg :weight bold))))
   `(ivy-minibuffer-match-face-2 ((t (:inherit isearch :weight bold))))
   
   ;; Diff faces
   `(diff-added ((t (:background ,beam-diff-added-bg :foreground ,beam-diff-added-fg))))
   `(diff-changed ((t (:background ,beam-diff-changed-bg :foreground ,beam-diff-changed-fg))))
   `(diff-removed ((t (:background ,beam-diff-removed-bg :foreground ,beam-diff-removed-fg))))
   `(diff-context ((t (:inherit diff-changed))))
   `(diff-file-header ((t (:inherit diff-added))))
   `(diff-function ((t (:inherit diff-added))))
   `(diff-header ((t (:inherit diff-added))))
   `(diff-hunk-header ((t (:inherit diff-added))))
   `(diff-index ((t (:inherit diff-added))))
   `(diff-indicator-added ((t (:inherit diff-added))))
   `(diff-indicator-changed ((t (:inherit diff-changed))))
   `(diff-indicator-removed ((t (:inherit diff-removed))))
   `(diff-nonexistent ((t (:background ,beam-diff-nonexistent-bg :foreground ,beam-diff-nonexistent-fg))))
   `(diff-refine-added ((t (:inherit diff-added))))
   `(diff-refine-changed ((t (:inherit diff-changed))))
   `(diff-refine-removed ((t (:inherit diff-removed))))

   ;; Whitespace faces
   `(whitespace-empty ((t (:background nil :foreground ,beam-whitespace-fg))))
   `(whitespace-indentation ((t (:background nil :foreground ,beam-whitespace-fg))))
   `(whitespace-line ((t (:background nil :foreground ,beam-whitespace-fg))))
   `(whitespace-newline ((t (:background nil :foreground ,beam-whitespace-fg))))
   `(whitespace-space ((t (:background nil :foreground ,beam-whitespace-fg))))
   `(whitespace-space-after-tab ((t (:background nil :foreground ,beam-whitespace-fg))))
   `(whitespace-space-before-tab ((t (:background nil :foreground ,beam-whitespace-fg))))
   `(whitespace-tab ((t (:background nil))))
   `(whitespace-trailing ((t (:background ,beam-whitespace-trailing-bg :foreground ,beam-whitespace-trailing-fg))))

   ;; Links
   `(link ((t (:foreground ,beam-link-fg :underline t :inherit bold))))
   `(link-visited ((t (:foreground ,beam-link-visited-fg :underline t :inherit bold))))   
   
   ;; Vertical border and info
   `(vertical-border ((t (:foreground ,beam-vertical-border-fg))))
   `(info ((t (:foreground ,beam-info-fg))))
   `(warning ((t (:foreground ,beam-warning-fg))))
   `(error ((t (:foreground ,beam-error-fg))))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'beam)
