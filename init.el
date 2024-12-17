;; -*- lexical-binding: t; -*-

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(setq custom-file "~/.emacs.d/custom.el")

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(require 'package)

(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org"          . "https://orgmode.org/elpa/")
                         ("elpa"         . "https://elpa.gnu.org/packages/")))

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/extra/simpc-mode")
(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.c\\'" . simpc-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . simpc-mode))

(set-default-coding-systems 'utf-8)

(use-package compile
  :init
  (progn
    (setq compilation-scroll-output t)
    (setq compilation-always-kill t)))

(use-package counsel
  :ensure t
  :diminish
  :bind
  ("C-x C-f" . counsel-find-file)
  :custom
  (counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:\\.\\(cs\\)?meta\\)")
  :init
  (counsel-mode 1))

(setq ido-ignore-extensions t)
(add-to-list 'completion-ignored-extensions ".meta")
(add-to-list 'completion-ignored-extensions ".cs.meta")

(use-package diminish
  :ensure t)

(use-package ivy
  :ensure t
  :diminish
  :bind
  (("C-s" . swiper)
   ("C-c v" . ivy-push-view)
   ("C-c V" . ivy-pop-view)
   ("M-SPC" . ivy-switch-view)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c L" . counsel-git-log)
   ("C-c C-r" . ivy-resume)
   ("C-c b" . counsel-bookmark)
   ("C-c d" . counsel-descbinds)
   ("C-c t" . counsel-load-theme)
   :map ivy-minibuffer-map
   ("<tab>"   . ivy-alt-done)
   ("<C-tab>" . ivy-partial-or-done)
   ("C-l"     . ivy-alt-done)
   ("C-i"     . ivy-immediate-done))
  :config
  (ivy-mode 1)
  (setq ivy-extra-directories nil)
  (setq ivy-use-selectable-prompt t))

(use-package ivy-prescient
  :ensure t
  :after counsel
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1)
  (setq prescient-sort-length-enable nil)
  (setq ivy-prescient-enable-filtering nil)
  (setq ivy-prescient-retain-classic-highlighting t))

(use-package eglot
  :ensure t
;  :hook (csharp-mode . eglot-ensure)
  :bind
  (("C-c r r" . eglot-rename)
  ("C-c e f"  . eglot-format)
  ("C-c e h"  . eglot-help-at-point)
  ("C-<f12>"  . xref-find-definitions)
  ("M-<f12>"  . xref-find-definitions-other-window)
  ("C-c f"    . xref-find-definitions)
  ("C-c F"    . xref-find-definitions-other-window)
  ("S-<f12>"  . xref-find-references)
  ("<f12>"    . xref-find-definitions))
  :init
  (setq eglot-ignored-server-capabilites
        '(:hoverProvider :completionProvider
                         :signatureHelpProvider :documentHighlightProvider
                         :documentSymbolProvider :workspaceSymbolProvider
                         :codeLensProvider :codeActionProvider 
                         :documentFormattingProvider :documentRangeFormattingProvider
                         :documentOnTypeFormattingProvider :documentLinkProvider
                         :colorProvider :foldingRangeProvider
                         :executeCommandProvider :inlayHintProvider))
  :config  
  (add-to-list 'eglot-server-programs `(csharp-mode . ("omnisharp" "-lsp")))
  :custom
  (eglot-connect-timeout 60))

;; Org-mode
(use-package org
  :ensure org
  :bind
  (:map org-mode-map
        ("C-<tab>" . org-cycle))
  :config
  (setq org-hide-emphasis-markers t)
  (setq org-log-done 'time)
  (setq org-return-follows-link  t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(p)" "INSPECT(i)" "|" "DONE(d)" "VERIFY(v)" "FIXED(f)" "CANCELLED(c)" "DEFERRED(w)" "INVALID(n)")
          ))
  (setq org-todo-keyword-faces
        '(
          ("TODO" . (:foreground "GoldenRod" :weight bold))
          ("INSPECT" . (:foreground "SlateBlue" :weight bold))
          ("INPROGRESS" . (:foreground "DodgerBlue" :weight bold))
          ("DONE" . (:foreground "LimeGreen" :weight bold))
          ("FIXED" . (:foreground "LimeGreen" :weight bold))
          ("VERIFY" . (:foreground "DarkGreen" :weight bold))
          ("DEFERRED" . (:foreground "GreenYellow" :weight bold))
          ("CANCELLED" . (:foreground "Red" :weight bold))
          ("INVALID" . (:foreground "DarkRed" :weight bold))
          ))
  (add-hook 'org-mode-hook 'org-indent-mode))

(setq completion-ignored-extensions
      (append completion-ignored-extensions
              '(".meta" ".cs.meta" ".csproj" ".sln" ".asmdef")))

(defun exec-bat-recursively (dir bat-file)
  "Find and run specified BAT file recursively up to the root directory."
  (let ((bat-file-path (concat dir bat-file)))
    (if (file-exists-p bat-file-path)
        (progn
          (message "Running %s file found at: %s with shell" bat-file bat-file-path)
          (shell-command (concat bat-file-path " &")))
      (when (not (string= dir "/"))
        (exec-bat-recursively (file-name-directory (directory-file-name dir)) bat-file)))))

(defun exec-bat-recursively-with-compile (dir bat-file)
  "Find and run specified BAT file recursively up to the root directory."
  (let ((bat-file-path (concat dir bat-file)))
    (if (file-exists-p bat-file-path)
        (progn
          (message "Running %s file found at: %s" bat-file bat-file-path)
          (compile bat-file-path))
      (when (not (string= dir "/"))
        (exec-bat-recursively-with-compile (file-name-directory (directory-file-name dir)) bat-file)))))

(defun exec-build-bat ()
  "Exec the build.bat file recursively up to the root directory."
  (interactive)
  (exec-bat-recursively-with-compile (file-name-directory buffer-file-name) "build.bat"))

(defun exec-debug-bat ()
  "Executes the build.bat file recursively with confirmation."
  (interactive)
  (when (y-or-n-p "Are you sure you want to execute debug.bat?")
    (exec-bat-recursively (file-name-directory buffer-file-name) "debug.bat")))

(defun behiri-find-file-other-window (&optional initial-input initial-directory)
  "Opens a file in other window using counsel.
   When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
  (interactive)
  (declare (lexical-binding t)) ; Add lexical-binding declaration
  (let ((default-directory (or initial-directory default-directory))
        (action (lambda (file)
                  (if (> (length (window-list)) 1)
                      (progn
                        (other-window 1)
                        (counsel-find-file-action file))
                    (progn
                      (select-window (split-window-right))
                      (counsel-find-file-action file))))))
    (counsel--find-file-1 "Find file other Window by behiri: " initial-input
                          action
                          'counsel-find-file)))


(defun behiri-copy-file-path ()
  "Copy the current buffer's file path to the kill ring."
  (interactive)
  (when buffer-file-name
    (kill-new buffer-file-name)
    (message "File path '%s' copied to the clipboard" buffer-file-name)))

(defun swap-window-positions ()
   "*Swap the positions of this window and the next one."
   (interactive)
   (let ((other-window (next-window (selected-window) 'no-minibuf)))
     (let ((other-window-buffer (window-buffer other-window))
           (other-window-hscroll (window-hscroll other-window))
           (other-window-point (window-point other-window))
           (other-window-start (window-start other-window)))
       (set-window-buffer other-window (current-buffer))
       (set-window-hscroll other-window (window-hscroll (selected-window)))
       (set-window-point other-window (point))
       (set-window-start other-window (window-start (selected-window)))
       (set-window-buffer (selected-window) other-window-buffer)
       (set-window-hscroll (selected-window) other-window-hscroll)
       (set-window-point (selected-window) other-window-point)
       (set-window-start (selected-window) other-window-start))
     (select-window other-window)))

(defvar behiri-change-buffer-keymap (make-sparse-keymap)
  "Keymap for behiri-change-buffer")

(define-key behiri-change-buffer-keymap (kbd "M-k") #'behiri-change-buffer-kill)

(defun behiri-filter-buffers ()
  "Filter buffers, ignoring temporary buffers."
  (cl-remove-if
   (lambda (buf)
     (or (string-prefix-p "*" (buffer-name buf))
         (and (not (buffer-file-name buf))
              (buffer-live-p buf))))
   (buffer-list)))


(defun behiri-change-buffer ()
  "Switch to a buffer, ignoring temporary buffers."
  (interactive)
  (let ((buffers (behiri-filter-buffers)))
    (ivy-read "Change buffer: "
              (mapcar #'buffer-name buffers)
              :preselect (buffer-name (other-buffer (current-buffer)))
              :keymap behiri-change-buffer-keymap
              :action (lambda (buffer)
                        (let ((selected-buffer (get-buffer buffer)))
                          (switch-to-buffer selected-buffer))))))

(defun behiri-change-buffer-other-window ()
  "Switch to a buffer on other window, ignoring temporary buffers."
  (interactive)
  (let ((buffers (behiri-filter-buffers)))
    (ivy-read "Change buffer on other window: "
              (mapcar #'buffer-name buffers)
              :preselect (buffer-name (current-buffer))
              :keymap behiri-change-buffer-keymap
              :action (lambda (buffer)
                        (let ((selected-buffer (get-buffer buffer)))
                          (switch-to-buffer-other-window selected-buffer))))))

(defun behiri-change-buffer-kill ()
  "Kill the current buffer in `behiri-change-buffer'."
  (interactive)
  (let ((selected-buffer (ivy-state-current ivy-last)))
    (ivy--kill-buffer-or-virtual selected-buffer)
    (unless (buffer-live-p (ivy-state-buffer ivy-last))
      (setf (ivy-state-buffer ivy-last)
            (with-ivy-window (current-buffer))))
    (setf (ivy-state-preselect ivy-last) ivy--index)
    (setq ivy--old-re nil)
    (setq ivy--all-candidates (delete selected-buffer ivy--all-candidates))
    (ivy--exhibit)))

(defun behiri-project-find-file-in-other-window ()
  "Open a file in the other window using project-find-file."
  (interactive)
  (let ((current-window (selected-window))
        (current-buffer (current-buffer))
        (new-buffer nil))
    (project-find-file)
    (unless (equal (current-buffer) current-buffer)
      (setq new-buffer (current-buffer))
      (cond
       ((= (length (window-list)) 1)
        (split-window-right)
        (other-window 1)
        (switch-to-buffer new-buffer)
        (select-window current-window)
        (switch-to-buffer current-buffer)
        (other-window 1))
       ((= (length (window-list)) 2)
        (other-window 1)
        (switch-to-buffer new-buffer)
        (select-window current-window)
        (switch-to-buffer current-buffer)
        (other-window 1))
       (t
        (switch-to-buffer new-buffer))))))

(defun behiri-save-all-buffers ()
  "Save all open files, ignoring temporary buffers."
  (interactive)
  (let ((saved-buffer-count 0))  ; Initialize the counter
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (buffer-file-name)  ; Check if buffer is associated with a file
                   (not (string-prefix-p " *" (buffer-name))))  ; Ignore temporary buffers starting with " *"
          (let ((file-name (buffer-file-name)))
            (when file-name
              ;; (message "Saving file: %s..." file-name)
              (save-buffer)
              (setq saved-buffer-count (1+ saved-buffer-count))
              ;; (sit-for 0.1)
              )
            (message nil)))))  ; Clear the message after each iteration
    (message "Saved %d buffer(s)." saved-buffer-count)))  ; Display the count

;; scroll config
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1)
(setq scroll-conservatively 10000
scroll-preserve-screen-position 1)

;; Revert buffers when the underlying file has changed
(setq auto-revert-interval 0.5)
(setq auto-revert-verbose t)
(global-auto-revert-mode 1)

;; Use spaces instead of tabs for indentation
(setq-default indent-tabs-mode nil)

(defun display-buffer-2-windows (buffer alist)
  "If only one window is available split it and display BUFFER there.
ALIST is the option channel for display actions (see `display-buffer')."
  (when (eq (length (window-list nil 'no-minibuf)) 1)
    (display-buffer--maybe-pop-up-window buffer alist)))

(setq display-buffer-base-action
      '((display-buffer--maybe-same-window
         display-buffer-reuse-window
         display-buffer--maybe-pop-up-frame
         display-buffer-2-windows
         display-buffer-in-previous-window
         display-buffer-use-some-window
         display-buffer-pop-up-frame)))

(setq undo-limit 20000)
(setq undo-strong-limit 40000)

(setq behiri-font "outline-Liberation Mono")

(setq default-frame-alist '((undecorated . t)))
(add-to-list 'default-frame-alist '(left-fringe . 0))
(add-to-list 'default-frame-alist '(right-fringe . 0))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 1))

;; Turn off the toolbar
(tool-bar-mode 0)

;; Set the backup directory
(setq backup-directory-alist `(("." . "~/.emacs-backups")))

;; Additional Ivy config options
(setq ivy-use-virtual-buffers t) ;; Enable recent files in switch buffer command
(setq enable-recursive-minibuffers t) ;; Allow commands in minibuffer recursively
(setq ivy-wrap t) ;; Wrap around candidate list

(load-library "view")
(require 'cc-mode)
(require 'ido)
(require 'compile)
(require 'use-package)

;; Setup my compilation mode
(defun behiri-big-fun-compilation-hook ()
  (make-local-variable 'truncate-lines)
  (setq truncate-lines nil))

(add-hook 'compilation-mode-hook 'behiri-big-fun-compilation-hook)

(defun load-todo ()
  (interactive)
  (find-file "w:/notes/todo.org"))

(defun insert-timeofday ()
  (interactive "*")
  (insert (format-time-string "---------------- %a, %d %b %y: %I:%M%p")))

(defun insert-timestamp ()
  (interactive "*")
  (insert (format-time-string "%Y-%m-%d %H:%M")))

(defun insert-signature ()
  (interactive)
  (insert "-behiri, " (format-time-string "%d %B %Y")))

(defun load-log ()
  (interactive)
  (find-file "w:/notes/log.org")
  (goto-char (point-max))
  (newline-and-indent)
  (insert "* ")
  (insert-timestamp)
  (newline-and-indent)
  (goto-char (point-max)))

;; Bright-red TODOs, lawn green NOTEs, and yellow @mentions
(setq fixme-modes '(c++-mode c-mode csharp-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(make-face 'font-lock-mention-face)

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)
           ("\\<\\(@[[:alnum:]_]+\\)" 1 'font-lock-mention-face t))))
      fixme-modes)

(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "LawnGreen" nil nil t nil t nil nil)
(modify-face 'font-lock-mention-face "Yellow" nil nil t nil t nil nil)

;; Accepted file extensions and their appropriate modes
(setq auto-mode-alist
      (append
       '(("\\.cpp$"    . c++-mode)
         ("\\.hpp$"    . c++-mode) 
         ("\\.h$"      . c-mode) 
         ("\\.c$"      . c-mode)
         ("\\.cc$"     . c-mode)
         ("\\.txt$"    . indented-text-mode)
         ("\\.emacs$"  . emacs-lisp-mode)
         ) auto-mode-alist))

;; C++ indentation style BigFun
(defconst behiri-big-fun-c-style
  '((c-electric-pound-behavior   . nil)
    (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
                                    (class-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
                                    (class-open)))

    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))
    
    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))

    (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                    (label                 . -4)
                                    (access-label          . -4)
                                    (substatement-open     .  0)
                                    (statement-case-intro  .  4)
                                    (statement-block-intro .  4) ;; c-lineup-for
                                    (case-label            .  4)
                                    (block-open            .  0)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     . -4)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  4)))
    (c-echo-syntactic-information-p . t))
  "Behiri's Big Fun C++ Style")


;; CC++ mode handling
(defun behiri-big-fun-c-hook ()
  ;; Set my style for the current buffer
  (c-add-style "BigFun" behiri-big-fun-c-style t)
  
  ;; 4-space tabs
  (setq tab-width 4
        indent-tabs-mode nil)

  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ;; No hungry backspace
  (c-toggle-auto-hungry-state -1)

  ;; Newline indents, semi-colon doesn't
  (define-key c++-mode-map "\C-m" 'newline-and-indent)
  (setq c-hanging-semi&comma-criteria '((lambda () 'stop)))

  ;; Abbrevation expansion
  (abbrev-mode 1)

  (defun behiri-header-format ()
    "Format the given file as a header file."
    (interactive)
    (let ((base-file-name (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
      (insert (format "#if !defined(%s_H)\n" (upcase base-file-name)))
      (insert "/* ========================================================================\n")
      (insert "   $File: $\n")
      (insert "   $Date: $\n")
      (insert "   $Revision: $\n")
      (insert "   $Creator: Behiri $\n")
      (insert "   $Notice: (C) Copyright 2024 by Behiri! All Rights Reserved. $\n")
      (insert "   ======================================================================== */\n\n")
      (insert (format "#define %s_H\n" (upcase base-file-name)))
      (insert "#endif")))

  (defun behiri-source-format ()
    "Format the given file as a source file."
    (interactive)
      (insert "/* ========================================================================\n")
      (insert "   $File: $\n")
      (insert "   $Date: $\n")
      (insert "   $Revision: $\n")
      (insert "   $Creator: Behiri $\n")
      (insert "   $Notice: (C) Copyright 2024 by Behiri! All Rights Reserved. $\n")
      (insert "   ======================================================================== */\n"))

  (cond ((file-exists-p buffer-file-name) t)
      ((string-match "[.]cs\\ |[.]h"            buffer-file-name) (behiri-header-format))
      ((string-match "[.]hin\\|[.]cin\\|[.]cpp" buffer-file-name) (behiri-source-format)))

  (defun open-corresponding-file ()
    "Open the corresponding header/source file for C/C++."
    (interactive)
    (let* ((base-file-name (file-name-sans-extension buffer-file-name))
           (corresponding-file-name
            (cond ((string-match "\\.c" buffer-file-name) (concat base-file-name ".h"))
                  ((string-match "\\.h" buffer-file-name) (concat base-file-name ".c"))
                  ((string-match "\\.cpp" buffer-file-name) (concat base-file-name ".h"))
                  ((string-match "\\.h" buffer-file-name) (concat base-file-name ".cpp"))
                  (t nil))))
      (if corresponding-file-name
          (find-file corresponding-file-name)
        (error "Unable to find a corresponding file"))))

  (defun open-corresponding-file-other-window ()
    "Find the file that corresponds to this one in another window."
    (interactive)
    (find-file-other-window buffer-file-name)
    (open-corresponding-file)
    (other-window -1))

  (define-key c++-mode-map [f12] 'open-corresponding-file)
  (define-key c++-mode-map [M-f12] 'open-corresponding-file-other-window)
  (define-key c++-mode-map (kbd "M-e") 'open-corresponding-file)
  (define-key c++-mode-map (kbd "M-E") 'open-corresponding-file-other-window)
  (define-key c++-mode-map (kbd "M-s") 'behiri-save-buffer)
  (define-key c++-mode-map (kbd "TAB") 'dabbrev-expand)
  (define-key c++-mode-map [S-tab] 'indent-for-tab-command)
  (define-key c++-mode-map (kbd "C-y") 'indent-for-tab-command)
  (define-key c++-mode-map (kbd "C-<tab>") 'indent-region)
  (define-key c++-mode-map (kbd "M-j") 'imenu)
  (define-key c++-mode-map (kbd "M-.") 'c-fill-paragraph)
  (define-key c++-mode-map (kbd "M-/") 'c-mark-function)
  (define-key c++-mode-map (kbd "M-q") 'append-as-kill)
  (define-key c++-mode-map (kbd "M-a") 'yank)
  (define-key c++-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region)
  
  ;; devenv.com error parsing
  (add-to-list 'compilation-error-regexp-alist 'behiri-devenv)
  (add-to-list 'compilation-error-regexp-alist-alist '(behiri-devenv
                                                       "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
                                                       2 3 nil (4))))

;; TODO ? (c-set-offset 'annotation-top-cont 0)

;;; Csharp mode hook
(defun behiri-csharp-mode-hook ()
  (setq c-default-style "BigFun")
  (setq c-basic-offset 4)
  (setq csharp-indent-offset 4)
  (c-set-offset 'statement-block-intro '+)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'topmost-intro-cont 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'statement-case-intro '+)
  (c-set-offset 'class-open 0)
  (c-set-offset 'class-close 0)
  (c-set-offset 'defun-open 0)
  (c-set-offset 'defun-close 0)
  (c-set-offset 'inher-intro 0)
  (c-set-offset 'access-label 0)
  (c-set-offset 'label 0)
  (define-key csharp-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))

(add-hook 'csharp-mode-hook 'behiri-csharp-mode-hook)

;;; Define a function to insert a Unity script class structure with dynamic class name
(defun insert-unity-script ()
  (when (and (string= (file-name-extension buffer-file-name) "cs")
             (zerop (buffer-size)))
    (let ((class-name (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
      (insert (format
               "using UnityEngine;

namespace
{
    public class %s : MonoBehaviour
    {
    }
}
" class-name)))))

(add-hook 'find-file-hook 'insert-unity-script)

;;; recenter the cursor vertically
(add-hook 'server-switch-hook (lambda () (recenter)))
(setq scroll-conservatively 21)
(setq scroll-preserve-screen-position t)

;;; Enable yank-advised-indent
(defadvice yank (after indent-region activate)
  (if (member major-mode '(emacs-lisp-mode lisp-mode csharp-mode  c-mode c++-mode plain-tex-mode))
      (indent-region (region-beginning) (region-end))))


(defun behiri-replace-string (FromString ToString)
  "Replace a string without moving point."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion
    ;;(replace-string FromString ToString)
    (while (search-forward FromString nil t)
      (replace-match ToString))))

(add-hook 'c-mode-common-hook 'behiri-big-fun-c-hook)

(defun behiri-save-buffer ()
  "Save the buffer after untabifying it."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (untabify (point-min) (point-max))))
  (save-buffer))

(defun append-as-kill ()
  "Performs copy-region-as-kill as an append."
  (interactive)
  (append-next-kill) 
  (copy-region-as-kill (mark) (point)))

(defun kill-current-buffer ()
  "Kill the current buffer without any prompts."
  (interactive)
  (kill-buffer (current-buffer)))

(defun kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun behiri-replace-in-region (old-word new-word)
  "Perform a replace-string in the current region."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion (save-restriction
                    (narrow-to-region (mark) (point))
                    (goto-char (point-min))
                    ;;(replace-string old-word new-word)
                    (while (search-forward old-word nil t)
                      (replace-match new-word)))))

;; Compilation
(setq compilation-context-lines 0)
(setq compilation-error-regexp-alist
      (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
            compilation-error-regexp-alist))

;; Commands
(set-variable 'grep-command "findstr -s -n -i -l \"\" -d .\\. *.c *.h *.cs")
(set-variable 'grep-command-position 22)
(setq counsel-grep-base-command "findstr -s -n -i -l \"%s\" *.c *.h *.cs")

;; Smooth scroll
(setq scroll-step 1) ;; 3

;; Clock
(display-time)

;; Startup windowing
(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(setq auto-save-default nil)
(setq auto-save-interval 0)
(setq auto-save-list-file-prefix nil)
(setq auto-save-timeout 0)
(setq auto-show-mode t)
(setq delete-auto-save-files nil)
(setq delete-old-versions 'other)
(setq imenu-auto-rescan t)
(setq imenu-auto-rescan-maxout 500000)
(setq kept-new-versions 5)
(setq kept-old-versions 5)
(setq make-backup-file-name-function 'ignore)
(setq make-backup-files nil)
(setq mouse-wheel-follow-mouse nil)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(15))
(setq package-selected-packages '(diminish ivy-prescient counsel))
(setq version-control nil)
(setq ring-bell-function 'ignore)
(setq set-mark-command-repeat-pop t)
(setq mark-ring-max 32)
(setq compilation-directory-locked nil)
(setq shift-select-mode t)
(setq enable-local-variables nil)
(setq split-height-threshold 80)
(setq split-width-threshold 1) ;; nil = split to top and down |  1 = split to right and left
(setq-default fill-column 95)
(setq display-line-numbers-width 3)

(setq dabbrev-case-replace nil)
(setq dabbrev-case-fold-search nil)
(setq visible-bell t)

;; (global-subword-mode 1)
(global-hl-line-mode -1)
(scroll-bar-mode -1)

(defun behiri-window-setup-hook ()
  (interactive)
  (menu-bar-mode -1)
  (setq inhibit-startup-message t)
  (toggle-scroll-bar -1) 
  (tool-bar-mode -1)
  (delete-selection-mode 1))

(add-hook 'window-setup-hook 'behiri-window-setup-hook t)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (when (window-system frame)
              (toggle-frame-fullscreen)
              (setq initial-buffer-choice nil)
              (behiri-window-setup-hook))))

;;; set theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'beam t)

;;; font
(add-to-list 'default-frame-alist '(font . "Liberation Mono-11"))
(set-face-attribute 'default t :font "Liberation Mono-11")

(defun set-font-size ()
  "Set the font size."
  (interactive)
  (set-face-attribute
   'default nil :height
   (string-to-number
    (read-string "Font size: " (number-to-string (face-attribute 'default :height nil))))))

;;; background color
(defun behiri-cycle-background-color ()
  (let ((color-index 0)
        (colors '("#072228" "#072822" "#2A282A" "#090909"
                  "#1c1c1c" "#202432" "#111122" "#040C08"
                  "#22212C" "#1E1D2D" "#181818" "#0D1712"
                  "#100E05" "#2A241D" "#1A140D"
                  )))
    ;; crt greens     "#223229" "#17221C" "#16231C"  "#1A2821"
    
    (lambda ()
      (interactive)
      (setq color-index (mod (1+ color-index) (length colors)))
      (let* ((color-code (nth color-index colors))
             (lighter-color (lighter-shade color-code)))
        (set-background-color color-code)
        (set-face-attribute 'mode-line-active nil :background lighter-color)
        (set-face-attribute 'mode-line-inactive nil :background lighter-color)
        (set-face-attribute 'mode-line nil :background lighter-color)
        (message "Background color set to %s. Modeline color set to %s" color-code lighter-color)
        (redraw-display)))))


;;; foreground color
(defun behiri-cycle-foreground-color ()
  (let ((color-index 0)
        (colors '("#bbff88" "#fff176" "#f5f5f5" "#20C16D" "#90ee90"
                  "#FFFFFF" "#D2B58C" "#E2C59C" "#00BB4E" "#cdd2ca")))
    (lambda ()
      (interactive)
      (setq color-index (mod (1+ color-index) (length colors)))
      (let* ((color-code (nth color-index colors)))
        (set-foreground-color color-code)
        (message "foreground color set to %s." color-code)
        (redraw-display)))))

(defun lighter-shade (color)
  "Generate a lighter shade of the input color in #RRGGBB format."
  (let* ((r (string-to-number (substring color 1 3) 16))
         (g (string-to-number (substring color 3 5) 16))
         (b (string-to-number (substring color 5 7) 16))
         (factor 0.023)
         (new-r (min (+ r (* 255 factor)) 255))
         (new-g (min (+ g (* 255 factor) 6) 255))
         (new-b (min (+ b (* 255 factor)) 255)))
    (format "#%02X%02X%02X" new-r new-g new-b)))

(defun search-with-baregrep ()
  "Prompt for a search string and use BareGrep to search in the specified path."
  (interactive)
  (let ((search-string (read-string "Enter search string: ")))
    (start-process "baregrep" "*BareGrep Output*"
                   "C:/Tools/BareGrep/baregrep.exe" search-string
                   (concat "*.c")   (concat "*.h")
                   (concat "*.cpp") (concat "*.cs"))))

;; global keybindings 
(global-set-key (kbd "C-c c")      (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "M-f")        'find-file)
(global-set-key (kbd "M-F")        'behiri-find-file-other-window)
;;(global-set-key (kbd "M-b")        'ido-switch-buffer)
;;(global-set-key (kbd "M-B")        'ido-switch-buffer-other-window)
(global-set-key (kbd "M-b")        'counsel-ibuffer)
(global-set-key (kbd "M-B")        'ibuffer-other-window)
(global-set-key (kbd "M-c")        'behiri-change-buffer)
(global-set-key (kbd "M-C")        'behiri-change-buffer-other-window)
(global-set-key (kbd "M-t")        'load-todo)
(global-set-key (kbd "M-T")        'load-log)
(global-set-key (kbd "M-w")        'other-window)
(global-set-key (kbd "M-2")        'swap-window-positions)
(global-set-key (kbd "C-<right>")  'forward-word)
(global-set-key (kbd "C-<left>")   'backward-word)
(global-set-key (kbd "C-<up>")     'backward-paragraph)
(global-set-key (kbd "M-<up>")     'backward-paragraph)
(global-set-key (kbd "C-<down>")   'forward-paragraph)
(global-set-key (kbd "M-<down>")   'forward-paragraph)
(global-set-key (kbd "<home>")     'beginning-of-line)
(global-set-key (kbd "<end>")      'end-of-line)
(global-set-key (kbd "C-<home>")   'move-beginning-of-line)
(global-set-key (kbd "C-<end>")    'move-end-of-line)
(global-set-key (kbd "<prior>")    'scroll-down-command)
(global-set-key (kbd "<next>")     'scroll-up-command)
(global-set-key (kbd "C-<next>")   'scroll-other-window)
(global-set-key (kbd "C-<prior>")  'scroll-other-window-down)
(global-set-key (kbd "M-v")        'pop-to-mark-command)
(global-set-key (kbd "M-q")        'append-as-kill)
(global-set-key (kbd "M-z")        'suspend-frame)
(global-set-key (kbd "M-<right>")  'forward-word)
(global-set-key (kbd "M-<left>")   'backward-word)
(global-set-key (kbd "M-:")        'View-back-to-mark)
(global-set-key (kbd "M-n")        'next-error)
(global-set-key (kbd "M-N")        'previous-error)
(global-set-key (kbd "M-g")        'goto-line)
(global-set-key (kbd "M-j")        'imenu)
(global-set-key (kbd "M-u")        'undo)
(global-set-key (kbd "M-6")        'upcase-word)
(global-set-key (kbd "M-^")        'capitalize-word)
(global-set-key (kbd "M-.")        'fill-paragraph)
(global-set-key (kbd "M-l")        'behiri-replace-in-region)
(global-set-key (kbd "M-o")        'query-replace)
(global-set-key (kbd "M-O")        'behiri-replace-string)
(global-set-key (kbd "M-<delete>") 'kill-word)
(global-set-key (kbd "M-[")        'start-kbd-macro)
(global-set-key (kbd "M-]")        'end-kbd-macro)
(global-set-key (kbd "M-'")        'call-last-kbd-macro)
(global-set-key (kbd "M-r")        'revert-buffer)
(global-set-key (kbd "M-k")        'kill-current-buffer)
(global-set-key (kbd "M-K")        'kill-all-buffers)
(global-set-key (kbd "M-s")        'behiri-save-buffer)
(global-set-key (kbd "M-S")        'behiri-save-all-buffers)
(global-set-key (kbd "<tab>")      'dabbrev-expand)
(global-set-key (kbd "S-<tab>")    'indent-for-tab-command)
(global-set-key (kbd "<backtab>")  'indent-for-tab-command)
(global-set-key (kbd "C-<tab>")    'indent-region)
(global-set-key (kbd "C-z")        'undo)
(global-set-key (kbd "C-f")        'yank)
(global-set-key (kbd "C-S-f")      'counsel-yank-pop) 
(global-set-key (kbd "C-q")        'kill-ring-save)
(global-set-key (kbd "<escape>")   'keyboard-escape-quit)
(global-set-key (kbd "C-c e r")    'eval-region)
(global-set-key (kbd "C-c e b")    'eval-buffer)
(global-set-key (kbd "C-c r t")    'string-rectangle)
(global-set-key (kbd "C-c r k")    'kill-rectangle)
(global-set-key (kbd "M-m")        'exec-build-bat)
(global-set-key (kbd "<f6>")       'exec-debug-bat)
(global-set-key (kbd "<f8>")       'exec-build-bat)
(global-set-key (kbd "<f9>")       'first-error)
(global-set-key (kbd "<f10>")      'previous-error)
(global-set-key (kbd "<f11>")      'next-error)
(global-set-key (kbd "C-0")        'delete-window)
(global-set-key (kbd "C-1")        'delete-other-windows)
(global-set-key (kbd "C-2")        'split-window-below)
(global-set-key (kbd "C-3")        'split-window-right)
(global-set-key (kbd "C-4")        'next-error)
(global-set-key (kbd "C-<f11>")    'toggle-frame-fullscreen)
(global-set-key (kbd "<insert>")   'yank)
(global-set-key (kbd "S-<insert>") 'counsel-yank-pop)
(global-set-key (kbd "C-<insert>") 'overwrite-mode)
(global-set-key (kbd "C-=")        'enlarge-window-horizontally)
(global-set-key (kbd "C--")        'shrink-window-horizontally)
(global-set-key (kbd "C-<f5>")     (behiri-cycle-background-color))
(global-set-key (kbd "C-<f6>")     (behiri-cycle-foreground-color))
(global-set-key (kbd "C-c s")      'grep)
(global-set-key (kbd "C-c S")      'counsel-grep)
(global-set-key (kbd "C-c C-c")    'comment-or-uncomment-region)
(global-set-key (kbd "M-p")        'project-find-file)
(global-set-key (kbd "M-P")        'behiri-project-find-file-in-other-window)
(global-set-key (kbd "C-v")        'nil)
(global-set-key (kbd "C-c C-s")    'nil)
(global-set-key (kbd "C-c i s")    'insert-signature)
(global-set-key (kbd "C-c i t")    'insert-timestamp)
(global-set-key (kbd "C-c i d")    'insert-timeofday)
(global-set-key (kbd "C-c p")      'behiri-copy-file-path)
(global-set-key (kbd "C-x F")      'display-fill-column-indicator-mode)
(global-set-key (kbd "M-1")        'Behiri-shell)
(global-set-key (kbd "M-3")        'shell-command)
(global-set-key (kbd "`")          'Behiri-shell)
(global-set-key (kbd "C-,")        'beginning-of-buffer)
(global-set-key (kbd "C-.")        'end-of-buffer)
(global-set-key (kbd "C-c l")      'global-display-line-numbers-mode)
(global-set-key (kbd "C-S-s")      'search-with-baregrep)
(global-set-key (kbd "C-S-d")      'duplicate-line)
(global-set-key (kbd "C-c m")      'compile)
(global-set-key (kbd "C-S-<up>")   (lambda () (interactive) (transpose-lines -1)))
(global-set-key (kbd "C-S-<down>") (lambda () (interactive) (transpose-lines 1)))

(defun Behiri-shell ()
  (interactive)
  (split-window-vertically (floor (* 0.66 (window-height))))
  (shell)
  (swap-window-positions))

(add-hook 'shell-mode-hook 'comint-mode)
(define-key comint-mode-map (kbd "M-<backspace>") 'comint-kill-input)
(define-key comint-mode-map (kbd "`") 'delete-window)
(define-key comint-mode-map (kbd "M-1") 'delete-window)
(define-key comint-mode-map (kbd "C-<tab>") 'comint-dynamic-complete-filename)

(setq load-file custom-file)
(setq load-file "~/.emacs.d/misc.el")

(setq gc-cons-threshold (* 2 1000 1000))
