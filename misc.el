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
