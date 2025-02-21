(defun behiri-export-org-to-markdown ()
  "Export the current Org buffer to a new Markdown file"
  (interactive)
  (let* ((buffer-name (buffer-name))
         (markdown-file (concat (file-name-base buffer-name) ".md")))
    (with-temp-buffer
      (insert-buffer-substring (get-buffer buffer-name))
      (goto-char (point-min))
      (while (re-search-forward "^#.*\n" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\)" nil t)
        (replace-match
         (concat
          (make-string (* 2 (1- (length (match-string 1)))) ? )
          "-")
         nil nil))
      (write-region (point-min) (point-max) markdown-file)
      (message "Exported Org buffer to %s" markdown-file))))

(defun behiri-copy-file-path ()
  "Copy the current buffer's file path to the kill ring."
  (interactive)
  (when buffer-file-name
    (kill-new buffer-file-name)
    (message "File path '%s' copied to the clipboard" buffer-file-name)))

(defun capitalize-first-letter (str)
  "Capitalize the first letter of STR."
  (if (and (not (string-empty-p str)) (stringp str))
      (concat (upcase (substring str 0 1)) (substring str 1))
    str))

;; >>> SNIPPET: Class template @Remove 
(defun insert-class-template ()
  "Insert a C++ class definition using the file name as the class name."
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
         (class-name (file-name-sans-extension file-name))
         (class-name (capitalize-first-letter class-name)))
    (insert (format "#ifndef %s_HPP\n#define %s_HPP\n\n" (upcase class-name) (upcase class-name)))
    ;;    (insert (format "class %s {\n" class-name))
    ;;    (insert (format "public:\n"))
    (insert (format "struct %s {\n" class-name))
    (insert (format "    %s();\n" class-name))
    (insert (format "    ~%s();\n" class-name))
    (insert "\n")
    (insert "    void clear();\n")
    (insert "    bool create();\n")
    (insert "    void update();\n")
    (insert "    void destroy();\n")
    (insert "};\n\n")
    (insert (format "%s::%s()  { clear(); };\n" class-name class-name))
    (insert (format "%s::~%s() { destroy(); };\n\n" class-name class-name))
    (insert (format "void %s::clear() {};\n" class-name))
    (insert (format "bool %s::create() { return true; };\n" class-name))
    (insert (format "void %s::update() {};\n" class-name))
    (insert (format "void %s::destroy()\n" class-name))
    (insert (format "{\n    clear();\n};\n" class-name))
    (insert (format "#endif // %s_HPP" (upcase class-name)))
    ))

(global-set-key (kbd "C-c i c") 'insert-class-template)

;; >>> SNIPPET: include guards @Remove 
(defun insert-include-guard ()
  "Insert an include guard at point."
  (interactive)
  (let ((guard-name (read-string "Enter guard name: ")))
    (insert (format "#ifndef %s\n#define %s\n\n\n#endif // %s\n"
                    guard-name
                    guard-name
                    guard-name))))
(global-set-key (kbd "C-c i g") 'insert-include-guard)

(defun insert-include-guard-if-hpp ()
  "Insert an include guard at the top of .hpp files."
  (when (and (string= (file-name-extension buffer-file-name) "hpp")
             (not (looking-at-p "#ifndef")))
    (insert-class-template)
;; uncomment to just inset include guard
;;    (let* ((guard-name (upcase (file-name-base buffer-file-name)))
;;           (guard (format "#ifndef %s_H\n#define %s_H\n\n\n#endif // %s_H\n"
;;                          guard-name guard-name guard-name)))
;;      (goto-char (point-min))
;;      (insert guard))
    ))

;; (add-hook 'find-file-hook 'insert-include-guard-if-hpp)


(defun behiri-header-format ()
  "Insert default header format"
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
         (current-date (format-time-string "%Y-%m-%d")))
    (insert "/* ========================================================================\n")
    (insert (format "   $File: %s$\n" file-name))
    (insert (format "   $Date: %s$\n" current-date))
    (insert "   $Revision: 1$\n")
    (insert "   $Author: Behiri$\n")
    (insert "   $Notice: (C) Copyright 2025 by Behiri! All Rights Reserved.$\n")
    (insert "   ======================================================================== */\n\n")
    ))

(defun behiri-cpp-header-format ()
  "Insert defualt C++ header format"
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
         (class-name (file-name-sans-extension file-name))
         (class-name (capitalize (downcase class-name)))
         (current-date (format-time-string "%Y-%m-%d")))
    (insert "/* ========================================================================\n")
    (insert (format "   $File: %s$\n" file-name))
    (insert (format "   $Date: %s$\n" current-date))
    (insert "   $Revision: 1$\n")
    (insert "   $Author: Behiri$\n")
    (insert "   $Notice: (C) Copyright 2025 by Behiri! All Rights Reserved.$\n")
    (insert "   ======================================================================== */\n\n")
    (insert (format "#ifndef %s_HPP\n#define %s_HPP\n\n" (upcase class-name) (upcase class-name)))
    (insert (format "struct %s {\n" class-name))
    (insert (format "    %s();\n"  class-name))
    (insert (format "    ~%s();\n" class-name))
    (insert "\n")
    (insert "    void clear();\n")
    (insert "    bool create();\n")
    (insert "    void update();\n")
    (insert "    void destroy();\n")
    (insert "};\n")
    (insert (format "#endif // %s_HPP" (upcase class-name)))
    ))

(defun behiri-cpp-source-format ()
  "Insert defualt C++ source format"
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
         (file-name-noex (downcase (file-name-sans-extension file-name)))
         (class-name (capitalize file-name-noex))
         (current-date (format-time-string "%Y-%m-%d")))
    (insert "/* ========================================================================\n")
    (insert (format "   $File: %s$\n" file-name))
    (insert (format "   $Date: %s$\n" current-date))
    (insert "   $Revision: 1$\n")
    (insert "   $Author: Behiri$\n")
    (insert "   $Notice: (C) Copyright 2025 by Behiri! All Rights Reserved.$\n")
    (insert "   ======================================================================== */\n\n")
    (insert (format "#include \"%s.hpp\" \n\n" file-name-noex))
    (insert (format "%s::%s()  { clear(); }\n" class-name class-name))
    (insert (format "void %s::destroy() {}" class-name)
    (insert (format "%s::~%s() { destroy(); }\n\n" class-name class-name))
    (insert (format "void %s::clear() {}\n" class-name))
    (insert (format "bool %s::create() { return true; }\n" class-name))
    (insert (format "void %s::update() {}\n" class-name)))
    ))

(defun behiri-cs-file-format ()
  "Insert defualt C# file format"
  (interactive)
    (let* ((file-name (file-name-nondirectory (buffer-file-name)))
           (class-name (file-name-sans-extension file-name))
           (class-name (capitalize (downcase class-name)))
           (current-date (format-time-string "%Y-%m-%d")))
      (insert "/* ========================================================================\n")
      (insert (format "   $File: %s$\n" file-name))
      (insert (format "   $Date: %s$\n" current-date))
      (insert "   $Revision: 1$\n")
      (insert "   $Author: Behiri$\n")
      (insert "   $Notice: (C) Copyright 2025 by Behiri! All Rights Reserved.$\n")
      (insert "   ======================================================================== */\n\n")
      (insert "using UnityEngine;\n\n")
      (insert "namespace name_space\n{\n")
      (insert (format "    public class %s : MonoBehaviour\n" class-name))
      (insert (format "    {\n    }\n}" class-name))    
      ))

(defun behiri-insert-format ()
  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]hpp\\|[.]hxx" buffer-file-name) (behiri-cpp-header-format))
        ((string-match "[.]cpp\\|[.]cxx" buffer-file-name) (behiri-cpp-source-format))
        ((string-match "[.]c\\|[.]h"     buffer-file-name) (behiri-header-format))
        ((string-match "[.]cs"           buffer-file-name) (behiri-cs-file-format))
        ))

(add-hook 'find-file-hook 'behiri-insert-format)

(defun goto-matching-brace ()
  "Move the cursor to the matching closing brace (})."
  (interactive)
  (let ((pos (point)))
    (if (search-forward "}" nil t)
        (backward-char)
      (goto-char pos))))

(global-set-key (kbd "C-c m") 'goto-matching-brace)

;; Create Project structure
(defun create-project (base-dir project-name)
  "Create a project folder in BASE-DIR with the name PROJECT-NAME. Add build.bat, run.bat, debug.bat, src, and bin folders. Create and open main.c in the src folder."
  (interactive
   (list (read-directory-name "Base directory: ")
         (read-string "Project name: ")))
  (let* ((project-path (expand-file-name project-name base-dir))
         (src-path   (expand-file-name "src"       project-path))
         (bin-path   (expand-file-name "bin"       project-path))
         (main-file  (expand-file-name "main.c"    src-path))
         (build-file (expand-file-name "build.bat" project-path))
         (run-file   (expand-file-name "run.bat"   project-path))
         (debug-file (expand-file-name "debug.bat" project-path)))
    ;; Create the project structure
    (condition-case err
        (progn
          ;; Create project, src, and bin directories
          (unless (file-directory-p project-path)
            (make-directory project-path t))
          (unless (file-directory-p src-path)
            (make-directory src-path t))
          (unless (file-directory-p bin-path)
            (make-directory bin-path t))
          ;; Create batch files
          (create-project-file build-file #'build-bat-content project-name)
          (create-project-file run-file #'run-bat-content project-name)
          (create-project-file debug-file #'debug-bat-content project-name)
          ;; Create and open main.c
          (create-project-file main-file #'main-c-content project-name)
          (find-file main-file) ;; Open the main.c file in Emacs
          (message "Project '%s' created at '%s'" project-name project-path))
      ;; Handle errors gracefully
      (error
       (message "Failed to create project: %s" (error-message-string err))))))

(defun create-project-file (file-path content-fn project-name)
  "Create a file at FILE-PATH, using CONTENT-FN to insert content specific to PROJECT-NAME, if it doesn't already exist."
  (unless (file-exists-p file-path)
    (with-temp-file file-path
      (funcall content-fn project-name))))

(defun build-bat-content (project-name)
  "Insert the content for build.bat specific to PROJECT-NAME."
  (insert "@echo off\n")
  (insert "setlocal\n")
  (insert "cd /D \"%~dp0\"\n")
  (insert "set COMPILE_FILES= ../src/main.c\n")
  (insert "set LINK_FILES= main.obj\n")
  (insert "pushd bin\n")
  (insert "echo ----------------------------------------------------------------\n")
  (insert "echo:\n")
  (insert "echo ^>^>^> Compiling...\n")
  (insert "echo:\n")
  (insert "cl -c -Zi -JMC -W3 -FC -DEBUG:FULL %COMPILE_FILES%\n")
  (insert "echo ^>^>^> Linking...\n")
  (insert "echo:\n")
  (insert "link -DEBUG -PDB:main.pdb -NODEFAULTLIB:msvcrtd.lib ^\n")
  (insert "%LINK_FILES% ^\n")
  (insert "Shell32.lib shell32.lib Opengl32.lib Glu32.lib ^\n")
  (insert "-ENTRY:mainCRTStartup -SUBSYSTEM:CONSOLE -MACHINE:x64 -INCREMENTAL:NO\n")
  (insert "echo ----------------------------------------------------------------\n")
  (insert "popd\n")
  (insert "echo Done building " project-name "\n"))

(defun run-bat-content (project-name)
  "Insert the content for run.bat specific to PROJECT-NAME."  
  (insert "@echo off\n")
  (insert "cd /D \"%~dp0\"\n")
  (insert "pushd bin\n")
  (insert ".\\main.exe\n")
  (insert "popd\n")
  (insert "echo running " project-name "...\n"))

(defun debug-bat-content (project-name)
  "Insert the content for debug.bat specific to PROJECT-NAME."
  (insert "@echo off\n")
  (insert "cd /D \"%~dp0\"\n")
  (insert "pushd bin\n")
  (insert "start devenv \/debugexe .\\main.exe\n")
  (insert "popd\n"))

(defun main-c-content (_project-name)
  "Insert a basic C program template into main.c."
  (insert "#include <stdio.h>\n\n")
  (insert "int main() {\n")
  (insert "    printf(\"Hello, world!\\n\");\n")
  (insert "    return 0;\n")
  (insert "}\n"))

;; auto delete obj files on cpp and hpp file save
(defun delete-obj-on-save ()
  "Delete the corresponding .obj file in the bin directory on save."
  (when (and (buffer-file-name)
             (string-match "\\.\\(cpp\\|hpp\\|h\\|c\\)\\'" (buffer-file-name)))
    (let* ((src-file (buffer-file-name))
           (base-name (file-name-base src-file))
           (src-dir (file-name-directory src-file))
           ;; Navigate from src to project root, then to bin
           (project-dir (file-name-directory (directory-file-name src-dir)))
           (obj-dir (expand-file-name "bin/" project-dir))
           (obj-file (expand-file-name (concat base-name ".obj") obj-dir)))
      (when (file-exists-p obj-file)
        (delete-file obj-file)
        (message "Deleted %s" obj-file))))
  )

(add-hook 'after-save-hook #'delete-obj-on-save)

;; Optional: ensure cl-lib is available (for cl-some)
(require 'cl-lib)

(defvar beam-camel-skip-prefixes '("b2" "SDL_" "imgui" ".")
  "List of identifier prefixes that should not be transformed from CamelCase to snake_case.")

(defun beam-convert-camel-to-snake (s)
  "Convert CamelCase string S to snake_case.
If S begins with an uppercase letter, preserve its letter cases
(inserting underscores between a lowercase letter/digit and an uppercase letter).
If S begins with a lowercase letter, then the entire output is lower-case.
If S starts with one of the prefixes defined in `beam-camel-skip-prefixes',
S is returned unchanged."
  (if (cl-some (lambda (prefix) (string-prefix-p prefix s))
               beam-camel-skip-prefixes)
      s
    (let* ((is-type (and (> (length s) 0)
                         (let ((c (aref s 0)))
                           (and (>= c ?A) (<= c ?Z)))))
           ;; Ensure the regexp is case-sensitive.
           (case-fold-search nil)
           (converted (replace-regexp-in-string
                       "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2" s)))
      (if is-type converted (downcase converted)))))

(defun beam-camel-to-snake-region (beg end)
  "Convert all CamelCase identifiers in the selected region to snake_case.
For each identifier:
- If it begins with an uppercase letter, its letter case is preserved
  (e.g. \"PhysicsSystem\" becomes \"Physics_System\").
- If it begins with a lowercase letter, the result is fully lower-cased
  (e.g. \"segmentLength\" becomes \"segment_length\").
Identifiers starting with any prefix in `beam-camel-skip-prefixes'
are left unchanged."
  (interactive "r")
  (save-excursion
    (save-restriction
      ;; Limit processing to the active region.
      (narrow-to-region beg end)
      (goto-char (point-min))
      ;; Match identifiers: letters, digits, and underscores.
      (while (re-search-forward "\\b\\([A-Za-z_][A-Za-z0-9_]*\\)\\b" nil t)
        (let* ((match (match-string 0))
               (converted (beam-convert-camel-to-snake match)))
          (when (not (string= match converted))
            (replace-match converted t t)))))))

(global-set-key (kbd "C-c r s")      'beam-camel-to-snake-region)

(defun beam-args-to-init-region (beg end)
  "Convert a comma-separated parameter list in the region into assignments.
For example, if the region contains:
    Chain *parent, vec position, vec size, float angle
this command will call `next-line` twice, then move to the beginning of the current line
and insert the following code there:
    this->parent = parent;
    this->position = position;
    this->size = size;
    this->angle = angle
The original region remains unchanged."
  (interactive "r")
  (let ((txt (buffer-substring-no-properties beg end))
        assignments)
    ;; Split the region on commas and process each parameter.
    (dolist (param (split-string txt ","))
      (let* ((trim (string-trim param))
             ;; Split on whitespace. For "Chain *parent" you get '("Chain" "*parent")
             (parts (split-string trim "[ \t]+"))
             (last (car (last parts)))
             ;; Remove any leading '*' from the variable name.
             (var (if (string-prefix-p "*" last)
                      (substring last 1)
                    last)))
        (push (format "    this->%s = %s;" var var) assignments)))
    ;; Reverse the list so that assignments remain in the original order.
    (setq assignments (reverse assignments))
    ;; Save the point so that the region remains unchanged.
    (save-excursion
      ;;(goto-char end)
      ;;(end-of-line)
      ;; Move two lines down.
      (forward-line 1)
      (forward-line 1)
      (beginning-of-line)
      ;; Insert the generated assignments.
      (insert (mapconcat 'identity assignments "\n") "\n\n")
      (indent-for-tab-command))))

(global-set-key (kbd "C-c r a")      'beam-args-to-init-region)

;; Define a variable to track the toggle state.
(defvar my-custom-faces-active nil
  "Non-nil when custom faces are active.")

(defun toggle-my-custom-faces ()
  "Toggle custom colors for several font-lock faces."
  (interactive)
  (if my-custom-faces-active
      ;; Revert custom faces.
      (progn
        (custom-set-faces
         '(font-lock-type-face ((t (:inherit default :foreground nil))))
         '(font-lock-keyword-face ((t (:inherit default :foreground nil))))
         '(font-lock-variable-name-face ((t (:inherit default :foreground nil))))
         '(font-lock-constant-face ((t (:inherit default :foreground nil)))))
        (setq my-custom-faces-active nil)
        (message "Custom faces reverted."))
    ;; Apply custom faces.
    (custom-set-faces
     '(font-lock-type-face ((t (:inherit default :foreground "SeaGreen1"))))
     '(font-lock-keyword-face ((t (:inherit default :foreground "gold"))))
     '(font-lock-variable-name-face ((t (:inherit default :foreground "burlywood2"))))
     '(font-lock-constant-face ((t (:inherit default :foreground "tomato")))))
    (setq my-custom-faces-active t)
    (message "Custom faces applied.")))

(global-set-key (kbd "C-c <f6>")      'toggle-my-custom-faces)

(defun my-move-to-end-and-newline ()
  "Move point to the end of the current line and insert a new line with proper indentation."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "M-<return>") 'my-move-to-end-and-newline)

(defun my-insert-line-above ()
  "Insert a new line above the current line and indent it properly."
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (forward-line -1) ;(previous-line)
  (indent-according-to-mode))

(global-set-key (kbd "C-<return>") 'my-insert-line-above)

(defun my-query-replace-region ()
  "Perform a query-replace in the entire buffer.
The string to be replaced is taken from the currently active region.
Prompts for a replacement string."
  (interactive)
  (if (use-region-p)
      (let ((from (buffer-substring-no-properties (region-beginning) (region-end))))
        ;; Deactivate the mark.
        (deactivate-mark)
        ;; Read the replacement string from the minibuffer.
        (let ((to (read-string (format "Replace '%s' with: " from))))
          ;; Do the query replace across the whole buffer.
          (query-replace from to nil (point-min) (point-max))))
    (message "No region selected.")))

;; Bind the function to M-O.
(global-set-key (kbd "M-O") 'my-query-replace-region)

;; 
;; (defvar my--last-line nil "Last line number tracked.")
;; 
;; (defun my-recenter-maybe ()
;;   (let ((current-line (line-number-at-pos)))
;;     (when (and my--last-line
;;                (> (abs (- current-line my--last-line))
;;                   (/ (window-body-height) 2)))
;;       (recenter))
;;     (setq my--last-line current-line)))
;; 
;; (add-hook 'post-command-hook #'my-recenter-maybe)
;; 
;; (defun my-recenter-advice (&rest _)
;;   (recenter))
;; 
;; (advice-add 'swiper    :after #'my-recenter-advice)
;; (advice-add 'goto-line :after #'my-recenter-advice)
;; ;; Add more commands as needed
;;

(defvar my--last-line nil "Last line number tracked.")

(defun my-recenter-maybe ()
  (let ((current-line (line-number-at-pos))
        (when (and my--last-line
                   (> (abs (- current-line my--last-line))
                      (/ (window-body-height) 2)))
          (let* ((window-start (window-start))
                 (window-end (window-end))
                 (top-threshold (+ window-start 0)) ;; Top 3 lines
                 (bottom-threshold (- window-end 1)) ;; Bottom 3 lines
                 (cursor-pos (point)))
            ;; Check if cursor is in the top 3 lines
            (when (<= cursor-pos top-threshold)
              (recenter))
            ;; Check if cursor is in the bottom 3 lines and there is text below
            (when (and (>= cursor-pos bottom-threshold)
                       (save-excursion
                         (goto-char window-end)
                         (unless (looking-at-p "^\\s-*$") ;; Don't recenter if only empty lines
                           (recenter))))))
          (setq my--last-line current-line)))

    (add-hook 'post-command-hook #'my-recenter-maybe)))

(defun my-recenter-advice (&rest _)
  (recenter))

(advice-add 'swiper       :after #'my-recenter-advice)
(advice-add 'goto-line    :after #'my-recenter-advice)
;; (advice-add 'other-window :after #'my-recenter-advice)

;; Add a custom regex to recognize 'file(line): info: breakpoint hit'
(add-to-list 'compilation-error-regexp-alist 'breakpoint-info)
(add-to-list 'compilation-error-regexp-alist-alist
             '(breakpoint-info "^\\(.*\\)(\\([0-9]+\\)): info: breakpoint hit$" 1 2 nil 0))

(defface my-log-face
  '((t :foreground "#FF88FF"))
  "Face for 'log:' messages in compilation mode.")

(add-hook 'compilation-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\blog: .*\\b" . 'my-log-face)))))
(put 'upcase-region 'disabled nil)

;; SDL3 docs shortcut
(defun open-sdl-wiki ()
  "Concatenate the SDL_KeyboardEvent wiki URL with content of the active region,
or prompt for a string if no region is selected, and open the result in the browser."
  (interactive)
  (let* ((base-url "https://wiki.libsdl.org/SDL3/")
         (append-string (if (use-region-p)
                            (buffer-substring-no-properties (region-beginning) (region-end))
                          (read-string "Enter string to append: "))))
    (browse-url (concat base-url append-string))))

(global-set-key (kbd "C-C w")     'open-sdl-wiki)


(defun lock-window ()
  "Mark the current window so that commands like `other-window` will skip it."
  (interactive)
  (set-window-parameter (selected-window) 'no-other-window t)
  (message "Current window is now locked."))

(defun unlock-window ()
  "Remove the non-targetable flag from the current window.
   Commands like `other-window` will now consider this window again."
  (interactive)
  (set-window-parameter (selected-window) 'no-other-window nil)
  (message "Current window is now unlocked."))

(defun toggle-window-lock ()
  "Toggle whether the current window is targetable.
   If the current window has the 'no-other-window parameter non-nil,
   set it to nil so that window commands like `other-window` consider it;
   otherwise, set the parameter to t."
  (interactive)
  (let ((win (selected-window)))
    (if (window-parameter win 'no-other-window)
        (progn
          (set-window-parameter win 'no-other-window nil)
          (message "Current window is now unlocked."))
      (set-window-parameter win 'no-other-window t)
      (message "Current window is now locked."))))

(global-set-key (kbd "C-c L") 'toggle-window-lock)
