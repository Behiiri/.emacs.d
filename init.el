;; -*- lexical-binding: t; -*-

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(set-default-coding-systems 'utf-8)

(setq completion-ignored-extensions
      (append completion-ignored-extensions
              '(".meta" ".cs.meta")))

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

(defun exec-bat ()
  "Start the recursive search for specified BAT file."
  (interactive)
  (let ((bat-file (read-string "Enter the BAT file name: ")))
    (exec-bat-recursively (file-name-directory buffer-file-name) bat-file)))

(defun exec-run-bat ()
  "Exec the build.bat file recursively up to the root directory."
  (interactive)
  (exec-bat-recursively-with-compile (file-name-directory buffer-file-name) "run.bat"))

(defun exec-build-bat ()
  "Exec the build.bat file recursively up to the root directory."
  (interactive)
  (exec-bat-recursively-with-compile (file-name-directory buffer-file-name) "build.bat"))

(defun exec-debug-bat ()
  "Exec the build.bat file recursively up to the root directory."
  (interactive)
  (exec-bat-recursively (file-name-directory buffer-file-name) "debug.bat"))

(defun exec-debug-bat-with-confirmation ()
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

;; scroll config
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
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

(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

(setq behiri-todo-file "w:/notes/todo.txt")
(setq behiri-log-file "w:/notes/log.txt")

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
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c L" . counsel-git-log)
   ("C-c C-r" . ivy-resume)
   ("C-c b" . counsel-bookmark)
   ("C-c d" . counsel-descbinds)
   ("C-c t" . counsel-load-theme)
   :map ivy-minibuffer-map
   ("<tab>" . ivy-alt-done)
   ("<C-tab>" . ivy-partial-or-done)
   ("C-l"   . ivy-alt-done)
   ("C-i"   . ivy-immediate-done)
   ("C-k"   . ivy-previous-line)
   ("C-j"   . ivy-next-line)
   :map ivy-switch-buffer-map
   ("C-k"   . ivy-previous-line)
   ("C-j"   . ivy-next-line))
  :config
  (ivy-mode 1)
  (setq ivy-extra-directories nil)
  (setq ivy-use-selectable-prompt t)
  )

(use-package ivy-prescient
  :ensure t
  :after counsel
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1)
  (setq prescient-sort-length-enable nil)
  ;; (setq prescient-filter-method '(literal regexp fuzzy))
  (setq ivy-prescient-enable-filtering nil)
  (setq ivy-prescient-retain-classic-highlighting t))

;; Additional Ivy config options
(setq ivy-use-virtual-buffers t) ; Enable recent files in switch buffer command
(setq enable-recursive-minibuffers t) ;;Allow commands in minibuffer recursively
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
  (find-file behiri-todo-file))

(defun insert-timeofday ()
  (interactive "*")
  (insert (format-time-string "---------------- %a, %d %b %y: %I:%M%p")))

(defun load-log ()
  (interactive)
  (find-file behiri-log-file)
  (goto-char (point-max))
  (newline-and-indent)
  (insert-timeofday)
  (newline-and-indent)
  (newline-and-indent)
  (goto-char (point-max)))

;; no screwing with my middle mouse button
(global-unset-key [mouse-2])

;; Bright-red TODOs
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

;; Accepted file extensions and their appropriate modes
(setq auto-mode-alist
      (append
       '(("\\.cpp$"    . c++-mode)
         ("\\.hin$"    . c++-mode)
         ("\\.cin$"    . c++-mode)
         ("\\.inl$"    . c++-mode)
         ("\\.rdc$"    . c++-mode)
         ("\\.h$"    . c++-mode)
         ("\\.c$"   . c++-mode)
         ("\\.cc$"   . c++-mode)
         ("\\.c8$"   . c++-mode)
         ("\\.txt$" . indented-text-mode)
         ("\\.emacs$" . emacs-lisp-mode)
         ("\\.gen$" . gen-mode)
         ("\\.ms$" . fundamental-mode)
         ("\\.m$" . objc-mode)
         ("\\.mm$" . objc-mode)
         ) auto-mode-alist))
;; C++ indentation style
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

  ;; Handle super-tabbify (TAB completes, shift-TAB actually tabs)
  ;;(setq dabbrev-case-replace t)
  ;;(setq dabbrev-case-fold-search t)
  ;;(setq dabbrev-upcase-means-case-search t)

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
        ((string-match "[.]hin" buffer-file-name) (behiri-source-format))
        ((string-match "[.]cin" buffer-file-name) (behiri-source-format))
        ((string-match "[.]h"   buffer-file-name) (behiri-header-format))
        ((string-match "[.]cpp" buffer-file-name) (behiri-source-format)))

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
  (define-key c++-mode-map (kbd "M-z") 'kill-region)

  ;; devenv.com error parsing
  (add-to-list 'compilation-error-regexp-alist 'behiri-devenv)
  (add-to-list 'compilation-error-regexp-alist-alist '(behiri-devenv
                                                       "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
                                                       2 3 nil (4))))

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

;; Navigation
(defun previous-blank-line ()
  "Moves to the previous line containing nothing but whitespace."
  (interactive)
  (search-backward-regexp "^[ \t]*\n"))

(defun next-blank-line ()
  "Moves to the next line containing nothing but whitespace."
  (interactive)
  (forward-line)
  (search-forward-regexp "^[ \t]*\n")
  (forward-line -1))

(defun append-as-kill ()
  "Performs copy-region-as-kill as an append."
  (interactive)
  (append-next-kill) 
  (copy-region-as-kill (mark) (point)))

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
(set-variable 'grep-command "findstr -s -n -i -l ")

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
(setq shift-select-mode nil)
(setq enable-local-variables nil)
(setq split-height-threshold 80)
(setq split-width-threshold 160)

(split-window-horizontally)
(global-hl-line-mode -1)
(scroll-bar-mode -1)

(add-to-list 'default-frame-alist '(font . "Liberation Mono-11"))
(add-to-list 'default-frame-alist '(background-color . "#001C14"))
(add-to-list 'default-frame-alist '(foreground-color . "#EEEE88"))
(add-to-list 'default-frame-alist '(hl-line-background-color . "#052215"))
(add-to-list 'default-frame-alist '(cursor-color . "firebrick"))

(set-face-attribute 'default t :font "Liberation Mono-11")
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-type-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")
(set-face-attribute 'region nil :background "#123")
(set-face-attribute 'ivy-current-match nil :background "#124" :foreground 'unspecified)
(set-face-background 'hl-line "#052215");

(defun set-font-size ()
  "Set the font size."
  (interactive)
  (set-face-attribute
   'default nil :height
   (string-to-number
    (read-string "Font size: " (number-to-string (face-attribute 'default :height nil))))))

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

(global-set-key (kbd "M-f")        'find-file)
(global-set-key (kbd "M-F")        'behiri-find-file-other-window)
(global-set-key (kbd "M-b")        'ido-switch-buffer)
(global-set-key (kbd "M-B")        'ido-switch-buffer-other-window)
(global-set-key (kbd "M-t")        'load-todo)
(global-set-key (kbd "M-T")        'load-log)
(global-set-key (kbd "M-w")        'other-window)
(global-set-key (kbd "C-<right>")  'forward-word)
(global-set-key (kbd "C-<left>")   'backward-word)
(global-set-key (kbd "C-<up>")     'previous-blank-line)
(global-set-key (kbd "C-<down>")   'next-blank-line)
(global-set-key (kbd "<home>")     'beginning-of-line)
(global-set-key (kbd "<end>")      'end-of-line)
(global-set-key (kbd "<prior>")    'scroll-down-command)
(global-set-key (kbd "<next>")     'scroll-up-command)
(global-set-key (kbd "C-<next>")   'scroll-other-window)
(global-set-key (kbd "C-<prior>")  'scroll-other-window-down)
(global-set-key (kbd "M-v")        'pop-to-mark-command)
(global-set-key (kbd "M-q")        'append-as-kill)
(global-set-key (kbd "M-a")        'yank)
(global-set-key (kbd "M-z")        'suspend-frame)
(global-set-key (kbd "M-<up>")     'previous-blank-line)
(global-set-key (kbd "M-<down>")   'next-blank-line)
(global-set-key (kbd "M-<right>")  'forward-word)
(global-set-key (kbd "M-<left>")   'backward-word)
(global-set-key (kbd "M-:")        'View-back-to-mark)
(global-set-key (kbd "M-n")        'next-error)
(global-set-key (kbd "M-N")        'previous-error)
(global-set-key (kbd "M-g")        'goto-line)
(global-set-key (kbd "M-j")        'imenu)
;; (global-Set-key (kbd "C-c")         nil)
;; (Global-set-key (kbd "C-e")        'rotate-yank-pointer)
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
(global-set-key (kbd "M-k")        'kill-this-buffer)
(global-set-key (kbd "M-s")        'behiri-save-buffer) ;; save-buffer
(global-set-key (kbd "<tab>")      'dabbrev-expand)
(global-set-key (kbd "S-<tab>")    'indent-for-tab-command)
(global-set-key (kbd "<backtab>")  'indent-for-tab-command)
(global-set-key (kbd "C-<tab>")    'indent-region)
(global-set-key (kbd "C-z")        'undo)
(global-set-key (kbd "C-f")        'yank)
(global-set-key (kbd "C-q")        'kill-ring-save)
(global-set-key (kbd "<escape>")   'keyboard-escape-quit)
(global-set-key (kbd "C-c e r")    'eval-region)
(global-set-key (kbd "C-c e b")    'eval-buffer)
(global-set-key (kbd "C-c r t")    'string-rectangle)
(global-set-key (kbd "C-c r k")    'kill-rectangle)
(global-set-key (kbd "M-m")        'exec-build-bat)
(global-set-key (kbd "<f5>")       'exec-bat)
(global-set-key (kbd "<f6>")       'exec-debug-bat-with-confirmation)
(global-set-key (kbd "<f8>")       'exec-build-bat)
(global-set-key (kbd "<f7>")       'exec-run-bat)
(global-set-key (kbd "<f9>")       'first-error)
(global-set-key (kbd "<f10>")      'previous-error)
(global-set-key (kbd "<f11>")      'next-error)
(global-set-key (kbd "C-0")        'delete-window)
(global-set-key (kbd "C-1")        'delete-other-windows)
(global-set-key (kbd "C-2")        'split-window-below)
(global-set-key (kbd "C-3")        'split-window-right)
(global-set-key (kbd "C-4")        'next-error)
(global-set-key (kbd "C-<f11>")    'toggle-frame-fullscreen)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
