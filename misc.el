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

;; >>> SNIPPET: Class template
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

;; >>> SNIPPET: include guards
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
  "Insert a C++ class definition using the file name as the class name."
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
  "Insert a C++ class definition using the file name as the class name."
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
  "Format the given file as a source file."
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
    (insert (format "%s::%s()  { clear(); }\n" class-name class-name))
    (insert (format "%s::~%s() { destroy(); }\n\n" class-name class-name))
    (insert (format "void %s::clear() {}\n" class-name))
    (insert (format "bool %s::create() { return true; }\n" class-name))
    (insert (format "void %s::update() {}\n" class-name))
    (insert (format "void %s::destroy()\n" class-name))
    (insert (format "{\n    clear();\n}" class-name))
    ))

(defun behiri-insert-format ()
  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]hpp\\|[.]hxx"   buffer-file-name) (behiri-cpp-header-format))
        ((string-match "[.]cpp\\|[.]cxx"   buffer-file-name) (behiri-cpp-source-format))
        ((string-match "[.]c\\|[.]h\\|[.]cs" buffer-file-name) (behiri-header-format))
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
