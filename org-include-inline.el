;; -*- lexical-binding: t; -*-

;;; org-include-inline.el --- Display #+INCLUDE contents inline in Org mode buffers.

;;; Commentary:
;; This package provides a minor mode for Org mode buffers that allows
;; for the inline display of content specified by #+INCLUDE directives.
;; It uses overlays to show the included content directly below the
;; #+INCLUDE line, without modifying the actual buffer content.
;; It also provides interactive functions to create #+INCLUDE directives.

;; AUTHOR: Yibie (gunshotbox@gmail.com)
;; VERSION: 0.1.0
;; DATE: 2025-05-08
;; KEYWORDS: org-mode, include, inline, overlay

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-src)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface org-include-inline-face
  '((((class color) (background light)) :background "black" :foreground "white" :extend t)
    (((class color) (background dark)) :background "black" :foreground "white" :extend t)
    (t :inherit default))
  "Face used for displaying inline included content."
  :group 'org-include-inline)

(defcustom org-include-inline-max-lines-to-display 500
  "Maximum number of lines to display from an included file.
If an included section is larger, it will be truncated."
  :type 'integer
  :group 'org-include-inline)

(defcustom org-include-inline-auto-enable-in-org-mode nil
  "Whether to automatically enable org-include-inline-mode in Org buffers.
When non-nil, org-include-inline-mode will be enabled for all Org mode buffers.
When nil, you need to manually enable the mode with M-x org-include-inline-mode."
  :type 'boolean
  :group 'org-include-inline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal Variables and Data Structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local org-include-inline--overlays nil
  "Buffer-local list of overlays created by org-include-inline-mode.")

(defvar org-include-inline--source-buffers nil
  "Alist of (source-file . org-buffers) pairs.
Each pair maps a source file to a list of org buffers that include it.")

(defvar org-include-inline--block-types
  '("src" "example" "export" ":custom")
  "List of common block types for #+INCLUDE directives.")

(defvar org-include-inline--common-languages
  '("emacs-lisp" "python" "sh" "C" "C++" "java" "javascript" "css" "html" "org" "latex")
  "List of common programming languages for src blocks.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-include-inline--parse-include-directive (line-text)
  "Parse an #+INCLUDE line and return a plist with info.
The line should be in format: 
#+INCLUDE: \"FILE\" [BLOCK-TYPE LANGUAGE] [:lines \"N-M\"] or
#+INCLUDE: \"FILE::HEADLINE\" or
#+INCLUDE: \"FILE::BLOCK-NAME\"

BLOCK-TYPE can be 'src', 'example', etc.
LANGUAGE is the source code language when BLOCK-TYPE is 'src' or 'export'."
  (let ((case-fold-search t)
        file type lines-spec headline-spec block-type language named-block
        line-after-file remainder-after-headline)

    ;; 1. Match #+INCLUDE: "FILE" part and get the rest of the line
    (if (string-match "^[ \t]*#\\+INCLUDE:[ \t]*\"\\([^\"]+\\)\"\\(.*\\)" line-text)
        (progn
          (setq file (match-string 1 line-text))
          (setq line-after-file (match-string 2 line-text))
          (message "DEBUG: Initial file match: %s, rest: %s" file line-after-file)

          ;; Check if this is a named block reference (file::block-name)
          (when (string-match "\\(.*\\)::\\([^:]+\\)\\'" file)
            (setq named-block (match-string 2 file)
                  file (match-string 1 file))
            (message "DEBUG: Found named block: %s in file: %s" named-block file))

          (when file
            (setq file (expand-file-name file (if buffer-file-name
                                                 (file-name-directory buffer-file-name)
                                               default-directory)))
            ;; Initially, the remainder for further parsing is everything after the file part
            (setq remainder-after-headline line-after-file)

            ;; 2. Try to parse block type and language
            (when (string-match "^[ \t]+\\([^ \t\n]+\\)\\(?:[ \t]+\\([^ \t\n]+\\)\\)?[ \t]*\\(.*\\)" line-after-file)
              (let ((first-param (match-string 1 line-after-file))
                    (second-param (match-string 2 line-after-file)))
                ;; Check if first parameter is a block type
                (unless (string-prefix-p ":" first-param)
                  (setq block-type first-param
                        language second-param
                        remainder-after-headline (match-string 3 line-after-file)))))

            ;; 3. Try to parse ::headline-spec from remainder
            (when (and remainder-after-headline
                      (string-match "^[ \t]*::\\(.+?\\)[ \t]*\\(.*\\)" remainder-after-headline))
              (setq headline-spec (string-trim (match-string 1 remainder-after-headline)))
              (setq remainder-after-headline (match-string 2 remainder-after-headline)))

            ;; 4. Try to parse :lines from remainder
            (when (and remainder-after-headline
                      (string-match "^[ \t]*:lines[ \t]+\"\\([0-9]+-\?[0-9]*\\)\"" remainder-after-headline))
              (setq lines-spec (match-string 1 remainder-after-headline)))

            ;; Determine type
            (cond
             (named-block (setq type :named-block))
             (headline-spec (setq type :headline))
             (lines-spec (setq type :lines))
             (t (setq type :lines) (setq lines-spec "1-")))

            (message "DEBUG: Final parse result - type: %s, named-block: %s" type named-block)
            `(:file ,file 
              :type ,type 
              :lines-spec ,lines-spec 
              :headline-spec ,headline-spec
              :block-type ,block-type
              :language ,language
              :named-block ,named-block
              :original-line ,line-text)))
      nil)))


(defun org-include-inline--fetch-file-lines (file lines-spec &optional block-type language)
  "Fetch specific lines from FILE based on LINES-SPEC (e.g., \"1-10\", \"5\").
If BLOCK-TYPE is provided, wrap content in appropriate block.
LANGUAGE is used when BLOCK-TYPE is 'src' or 'export'.
Note that for 'example', 'export', or 'src' blocks, content is escaped."
  (unless (file-readable-p file)
    (message "Error: File not readable: %s" file)
    (return-from org-include-inline--fetch-file-lines (format "Error: File not readable: %s" file)))

  (let ((start-line 1)
        (end-line most-positive-fixnum)
        (content ""))
    (when lines-spec ; lines-spec could be nil if we default to full file but user provides no :lines
      (cond
       ((string-match "\\`\\([0-9]+\\)-\\([0-9]+\\)\\'" lines-spec) ; Match full spec "S-E"
        (setq start-line (string-to-number (match-string 1 lines-spec)))
        (setq end-line (string-to-number (match-string 2 lines-spec))))
       ((string-match "\\`\\([0-9]+\\)-\\'" lines-spec) ; Match "S-"
        (setq start-line (string-to-number (match-string 1 lines-spec))))
       ((string-match "\\`\\([0-9]+\\)\\'" lines-spec) ; Match "S" (single line)
        (setq start-line (string-to-number (match-string 1 lines-spec)))
        (setq end-line start-line))
       ((string-equal lines-spec "1-") ; Explicit full file through "1-"
         (setq start-line 1 end-line most-positive-fixnum))
       (t (message "Warning: Invalid lines spec: \"%s\" for file %s. Defaulting to all lines." lines-spec file)
          (setq start-line 1 end-line most-positive-fixnum))))

    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((current-line 1)
            (lines-count 0)
            (result-lines '()))
        (while (and (not (eobp)) (< current-line start-line))
          (forward-line 1)
          (setq current-line (1+ current-line)))
        (while (and (not (eobp)) 
                    (<= current-line end-line) 
                    (< lines-count org-include-inline-max-lines-to-display))
          (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) 
                result-lines)
          (forward-line 1)
          (setq current-line (1+ current-line)
                lines-count (1+ lines-count)))
        (setq content (mapconcat #'identity (nreverse result-lines) "\n"))
        (when (and (not (eobp)) (<= current-line end-line)) ; Means it was truncated
          (setq content (concat content 
                              (format "\n... (truncated at %d lines)" 
                                     org-include-inline-max-lines-to-display))))))
    
    ;; Wrap content in appropriate block if needed
    (when (and block-type (not (string-empty-p content)))
      (let* ((block-name (if (string-prefix-p "\"" block-type)
                            (substring block-type 1 -1)
                          block-type))
             (needs-escape (member block-name '("src" "example" "export")))
             (escaped-content (if needs-escape
                                (org-escape-code-in-string content)
                              content)))
        (setq content
              (cond
               ((member block-name '("src" "export"))
                (format "#+begin_%s %s\n%s\n#+end_%s" 
                        block-name
                        (or language "")
                        escaped-content
                        block-name))
               (t
                (format "#+begin_%s\n%s\n#+end_%s"
                        block-name
                        escaped-content
                        block-name))))))
    content))

(defun org-include-inline--fetch-named-block-content (file block-name)
  "Fetch content of a named block from FILE.
BLOCK-NAME is the name of the block to fetch.
Returns only the content of the block, without the #+NAME:, #+begin_src, and #+end_src lines."
  (unless (file-readable-p file)
    (message "Error: File not readable: %s" file)
    (return-from org-include-inline--fetch-named-block-content 
                 (format "Error: File not readable: %s" file)))
  
  (message "DEBUG: Fetching named block: %s from file: %s" block-name file)
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let* ((ast (org-element-parse-buffer))
           (block (org-element-map ast '(src-block example-block)
                   (lambda (element)
                     (when (string= (org-element-property :name element) block-name)
                       element))
                   nil t))) ; stop at first match
      (message "DEBUG: Found block: %s" (if block "yes" "no"))
      (if block
          (let* ((content (org-element-property :value block)))
            (if content
                (let ((trimmed (string-trim content)))
                  (message "DEBUG: Block content length: %d" (length trimmed))
                  trimmed)
              "")) ; Return empty string if no content
        (format "Error: Named block \"%s\" not found in %s" 
                block-name (file-name-nondirectory file))))))

;; (defun org-include-inline--fetch-org-headline-content (file headline-spec)
;;   "Fetch content of a specific headline from an Org FILE.
;; HEADLINE-SPEC can be like \"*Headline Text\" or \"#custom-id\"."
;;   (unless (file-readable-p file)
;;     (message "Error: Org file not readable: %s" file)
;;     (return-from org-include-inline--fetch-org-headline-content (format "Error: Org file not readable: %s" file)))
;;   (unless headline-spec
;;       (message "Error: No headline specification provided for file %s" file)
;;       (return-from org-include-inline--fetch-org-headline-content (format "Error: No headline specified for %s" file)))

;;   (with-temp-buffer
;;     (insert-file-contents file)
;;     (org-mode) ; Ensure Org mode is active for parsing
;;     (let ((elements (org-element-parse-buffer 'greater-element))
;;           (target-headline nil)
;;           (content ""))
;;       ;; Find the headline
;;       (org-element-map elements 'headline
;;         (lambda (headline)
;;           (when target-headline (org-element-map-break)) ; Found it in a previous iteration
;;           (let ((title (org-element-property :title headline))
;;                 (custom-id (org-element-property :CUSTOM_ID headline))
;;                 (raw-headline (org-element-property :raw-value headline))) ; For *Headline matching
;;             (cond
;;              ;; Match by CUSTOM_ID (e.g., #custom-id)
;;              ((and (string-prefix-p "#" headline-spec)
;;                    custom-id
;;                    (string= custom-id (substring headline-spec 1)))
;;               (setq target-headline headline))
;;              ;; Match by full headline text (e.g., *Headline Text or ** Another)
;;              ((and (string-prefix-p "*" headline-spec)
;;                    raw-headline ; compare with the raw headline string from Org
;;                    (string= (substring headline-spec (cl-position ?* headline-spec :from-end t)) ; Get text after last *
;;                             (replace-regexp-in-string "^\\*+\\s-*" "" raw-headline))) ; Compare title part only
;;               (setq target-headline headline))
;;              ;; Match by named element (e.g. "my-element" for #+NAME: my-element)
;;              ;; This requires org-element-map to also check for 'named-element' types
;;              ;; or a different strategy if `headline-spec` does not start with * or #.
;;              ;; For now, we assume headline-spec implies * or # if it's a headline include.
;;              )))
;;         nil nil t) ; Search affiliated keywords

;;       (if target-headline
;;           (let* ((contents-begin (org-element-property :contents-begin target-headline))
;;                  (contents-end (org-element-property :contents-end target-headline))
;;                  (raw-content (if (and contents-begin contents-end)
;;                                   (buffer-substring-no-properties contents-begin contents-end)
;;                                 "")))
;;             ;; *** REPLACED complex regexp with string-trim ***
;;             (setq content (string-trim raw-content))

;;             ;; Basic truncation for very long headline contents (after trimming)
;;             (if (> (length content) (* org-include-inline-max-lines-to-display 80)) ; Approx char limit
;;                 (setq content (concat (substring content 0 (* org-include-inline-max-lines-to-display 80))
;;                                       "\n... (headline content truncated)"))))
;;         (setq content (format "Error: Headline/target \"%s\" not found in %s" headline-spec (file-name-nondirectory file)))))
;;       content))

(defun org-include-inline--create-or-update-overlay (point content &optional buffer)
  "Create or update an overlay at POINT to display CONTENT in BUFFER.
If BUFFER is nil, use current buffer. Ensures overlay stays attached to buffer."
  (let ((buf (or buffer (current-buffer))))
    (message "org-include-inline--create-or-update-overlay: ===ENTERED=== point=%s, content-len=%s, buffer=%s"
             point (if content (length content) "NIL") buf)

    (if (and content (> (length content) 0))
        (progn
          (message "org-include-inline--create-or-update-overlay: Content valid (len %d). Pt %s (buf: %s, min: %d, max: %d)."
                   (length content) point buf (with-current-buffer buf (point-min)) (with-current-buffer buf (point-max)))

          (condition-case e-make-overlay
              (with-current-buffer buf
                (let ((ov (make-overlay point point buf)))
                  (message "org-include-inline--create-or-update-overlay: make-overlay SUCCESS: ov=%S" ov)

                  (overlay-put ov 'org-include-inline t)
                  (overlay-put ov 'face 'org-include-inline-face)
                  (overlay-put ov 'after-string (propertize content 'face 'org-include-inline-face))
                  (overlay-put ov 'evaporate nil) ;; 改为 nil，防止自动删除
                  (message "org-include-inline--create-or-update-overlay: 'modification-hooks deliberately SKIPPED.")
                  (overlay-put ov 'priority 100)
                  (push ov org-include-inline--overlays)
                  (message "org-include-inline--create-or-update-overlay: Overlay pushed. Total: %d." (length org-include-inline--overlays))))
            (error 
             (message "org-include-inline--create-or-update-overlay: ERROR on make-overlay (point %s, buffer %s): %S" 
                      point buf e-make-overlay))))
      (message "org-include-inline--create-or-update-overlay: Content was NIL or empty. No overlay created. ===EXITING==="))))

(defun org-include-inline--clear-overlays ()
  "Remove all org-include-inline overlays from the current buffer."
  (when (and (boundp 'org-include-inline--overlays)
             org-include-inline--overlays)
    (dolist (ov org-include-inline--overlays)
      (when (overlayp ov)  
        (delete-overlay ov)))
    (setq org-include-inline--overlays nil)))

(defun org-include-inline--cleanup-on-kill ()
  "Clean up org-include-inline registrations when a buffer is killed."
  (when org-include-inline-mode
    (org-include-inline--unregister-buffer (current-buffer))))

(add-hook 'kill-buffer-hook #'org-include-inline--cleanup-on-kill)

(defun org-include-inline--register-source-file (source-file org-buffer)
  "Register that ORG-BUFFER includes SOURCE-FILE."
  (let* ((source-path (expand-file-name source-file))
         (existing-entry (assoc source-path org-include-inline--source-buffers)))
    (if existing-entry
        (unless (memq org-buffer (cdr existing-entry))
          (setcdr existing-entry (cons org-buffer (cdr existing-entry))))
      (push (cons source-path (list org-buffer)) 
            org-include-inline--source-buffers))))

(defun org-include-inline--unregister-buffer (buffer)
  "Remove BUFFER from all source file registrations."
  (setq org-include-inline--source-buffers
        (cl-loop for (source-file . buffers) in org-include-inline--source-buffers
                 for new-buffers = (remq buffer buffers)
                 when new-buffers  ; 只保留还有其他 buffer 的条目
                 collect (cons source-file new-buffers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-include-inline-toggle-visibility ()
  "Toggle visibility of inline included content."
  (interactive)
  (if (null org-include-inline--overlays)
      (org-include-inline-refresh-buffer) ; If none, try to create them
    (let* ((first-ov (car org-include-inline--overlays))
           (currently-visible (not (eq 'hidden (overlay-get first-ov 'display)))))
      (message "Toggle visibility: currently %s" (if currently-visible "visible" "hidden"))
      (dolist (ov org-include-inline--overlays)
        (when (overlayp ov)
          (if currently-visible
              (progn
                (overlay-put ov 'saved-after-string (overlay-get ov 'after-string))
                (overlay-put ov 'after-string nil)
                (overlay-put ov 'display 'hidden))
            (overlay-put ov 'after-string (overlay-get ov 'saved-after-string))
            (overlay-put ov 'display nil)))))))

(defun org-include-inline-refresh-buffer ()
  "Refresh all inline includes in the current buffer.

This function:
1. Clears all existing overlays
2. Scans for #+INCLUDE directives
3. For each directive:
   - Parses the include specification
   - Fetches content from source file
   - Creates overlay to display content
4. Registers source file dependencies for auto-refresh"
  (interactive)
  (message "Refreshing inline includes...")
  (org-include-inline--clear-overlays)
  
  ;; Only process if we're in an org buffer with the mode enabled
  (when (and (derived-mode-p 'org-mode) 
             org-include-inline-mode)
    (let ((current-buffer (current-buffer))
          (count 0)
          (source-files '()))  ; Track source files for this buffer
      
      (save-excursion
        (goto-char (point-min))
        ;; Find all #+INCLUDE directives
        (while (search-forward-regexp "^[ \t]*#\\+INCLUDE:" nil t)
          (setq count (1+ count))
          
          (let* ((current-line-text
                  (buffer-substring-no-properties 
                   (line-beginning-position) (line-end-position)))
                 (include-info (org-include-inline--parse-include-directive current-line-text)))
            
            (when include-info
              (let ((source-file (plist-get include-info :file)))
                ;; Add to source files list if not already there
                (unless (member source-file source-files)
                  (push source-file source-files))
                
                ;; Calculate overlay position (next line after #+INCLUDE)
                (let ((overlay-pos (save-excursion
                                   (forward-line 1)
                                   (point))))
                  
                  ;; Fetch and display content based on include type
                  (let ((content
                         (cond
                          ((eq (plist-get include-info :type) :lines)
                           (org-include-inline--fetch-file-lines 
                            source-file
                            (plist-get include-info :lines-spec)
                            (plist-get include-info :block-type)
                            (plist-get include-info :language)))
                          ((eq (plist-get include-info :type) :headline)
                           (org-include-inline--fetch-org-headline-content 
                            source-file
                            (plist-get include-info :headline-spec)))
                          ((eq (plist-get include-info :type) :named-block)
                           (org-include-inline--fetch-named-block-content
                            source-file
                            (plist-get include-info :named-block)))
                          (t 
                           (format "Error: Unknown include type for %s" 
                                 (plist-get include-info :original-line))))))
                    
                    ;; Create overlay if we have valid content
                    (when (and content (> (length content) 0))
                      (org-include-inline--create-or-update-overlay 
                       overlay-pos content current-buffer)
                      
                      ;; Register this org buffer as dependent on the source file
                      (org-include-inline--register-source-file 
                       source-file current-buffer)))))))))
      
      (message "Refresh complete. Processed %d includes." count))))

(defun org-include-inline--refresh-dependent-buffers ()
  "Refresh all org buffers that include the current buffer's file."
  (when buffer-file-name  
    (let ((source-path (expand-file-name buffer-file-name)))
      (dolist (buffer-entry org-include-inline--source-buffers)
        (when (string= (car buffer-entry) source-path)
          (dolist (org-buffer (cdr buffer-entry))
            (when (buffer-live-p org-buffer)
              (with-current-buffer org-buffer
                (org-include-inline-refresh-buffer)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive #+INCLUDE Creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar org-include-inline--confirm-lines-map (make-sparse-keymap)
  "Keymap for org-include-inline-confirm-lines in target buffer.")
(define-key org-include-inline--confirm-lines-map (kbd "C-c C-l") 'org-include-inline-confirm-lines)

(defun org-include-inline-insert-from-lines ()
  "Interactively select a file and a range of lines to create an #+INCLUDE directive."
  (interactive)
  (let* ((current-org-buffer (current-buffer))
         (target-file (read-file-name "Include lines from file: " nil nil t)))
    (unless (file-exists-p target-file)
      (message "File does not exist: %s" target-file)
      (error "File not found"))
    (let ((smart-path (org-include-inline--get-smart-path target-file)))
      (message "DEBUG: Target file: %s, smart path: %s" target-file smart-path)
      (let ((target-buffer (find-file-noselect target-file)))
        (unless target-buffer
          (error "Failed to open target file: %s" target-file))
        (with-current-buffer target-buffer
          (message "Setting up for file selection in: %s" (buffer-name))
          (set (make-local-variable 'org-include-inline--target-org-buffer) current-org-buffer)
          (set (make-local-variable 'org-include-inline--relative-target-file) smart-path)
          (unless (and (boundp 'org-include-inline--target-org-buffer)
                       (boundp 'org-include-inline--relative-target-file))
            (error "Failed to set required variables"))
          (use-local-map org-include-inline--confirm-lines-map)
          (message "Select region in %s, then press C-c C-l to confirm selection" 
                   (file-name-nondirectory target-file)))
        (select-window (display-buffer target-buffer 
                                      '(display-buffer-reuse-window display-buffer-pop-up-window)))))))

(defun org-include-inline-confirm-lines ()
  "Confirm line selection and insert #+INCLUDE directive. Called from the target file buffer."
  (interactive)
  (unless (and (boundp 'org-include-inline--target-org-buffer)
               (buffer-live-p org-include-inline--target-org-buffer))
    (error "No pending include operation. Use 'org-include-inline-insert-from-lines' first."))
  (unless (boundp 'org-include-inline--relative-target-file)
    (error "Target file path not found. Please restart the include operation."))
  (unless (region-active-p)
    (use-local-map nil) ; remove temporary keymap
    (error "No region selected. Please select lines to include."))
  (let ((start (line-number-at-pos (region-beginning)))
        (end (line-number-at-pos (region-end)))
        (rel-path org-include-inline--relative-target-file) 
        (org-buf org-include-inline--target-org-buffer))    
    (message "DEBUG: Creating include with rel-path: %s, lines: %d-%d" rel-path start end)
    (with-current-buffer org-buf
      (let ((actual-include-path rel-path))
        (message "DEBUG: Inserting INCLUDE directive with path: %s" actual-include-path)
        (insert (format "#+INCLUDE: \"%s\" :lines \"%d-%d\"\n" actual-include-path start end))
        (when org-include-inline-mode
          (org-include-inline-refresh-buffer))))
    (message "Successfully inserted #+INCLUDE for lines %d-%d from %s" start end rel-path)
    (use-local-map nil) ; remove temporary keymap
    (when (boundp 'org-include-inline--target-org-buffer)
      (kill-local-variable 'org-include-inline--target-org-buffer))
    (when (boundp 'org-include-inline--relative-target-file) 
      (kill-local-variable 'org-include-inline--relative-target-file))
    (if (one-window-p)
        (bury-buffer)
      (delete-window))))

(defun org-include-inline--get-smart-path (file)
  "Intelligently process file paths, prioritize using tilde to represent the user's home directory.
If FILE is located in the user's home directory, return the path starting with ~.
Otherwise, return the path relative to the current file."
  (let* ((expanded-file (expand-file-name file))
         (home-dir (expand-file-name "~"))
         (current-dir (if buffer-file-name
                          (file-name-directory buffer-file-name)
                        default-directory))
         (tilde-path (and (string-prefix-p home-dir expanded-file)
                          (concat "~" (substring expanded-file (length home-dir)))))
         (relative-path (file-relative-name expanded-file current-dir)))
    (cond
     ((and tilde-path (< (length tilde-path) (length relative-path)))
      tilde-path)
     ((and relative-path (not (string-equal relative-path ".")))
      relative-path)
     (t expanded-file))))

(defun org-include-inline-insert-file ()
  "Interactively select a file to create an #+INCLUDE directive for the entire file."
  (interactive)
  (let* ((target-file (read-file-name "Include entire file: " nil nil t))
         (smart-path nil))
    (unless (file-exists-p target-file)
      (message "File does not exist: %s" target-file)
      (error "File not found"))
    (setq smart-path (org-include-inline--get-smart-path target-file))
    (insert (format "#+INCLUDE: \"%s\"\n" smart-path))
    (message "Inserted #+INCLUDE for entire file: %s" smart-path)
    (when org-include-inline-mode
      (org-include-inline-refresh-buffer))))

(defun org-include-inline-insert-as-block ()
  "Interactively create an #+INCLUDE directive with a specific block type.
This allows including file content wrapped in src, example, or other blocks."
  (interactive)
  (let* ((target-file (read-file-name "Include file as block: " nil nil t))
         (smart-path nil)
         (block-type (completing-read "Block type: " org-include-inline--block-types nil nil))
         (language nil))
    
    ;; Validate file exists
    (unless (file-exists-p target-file)
      (message "File does not exist: %s" target-file)
      (error "File not found"))
    
    ;; Get the smart path (relative or with ~)
    (setq smart-path (org-include-inline--get-smart-path target-file))
    
    ;; For src blocks, prompt for language
    (when (string= block-type "src")
      (setq language (completing-read "Programming language: " 
                                    org-include-inline--common-languages
                                    nil ; predicate
                                    nil ; require-match
                                    nil ; initial-input
                                    nil ; hist
                                    (when-let* ((file-ext (file-name-extension target-file))
                                              (lang (cond
                                                    ((member file-ext '("el" "elisp")) "emacs-lisp")
                                                    ((member file-ext '("py")) "python")
                                                    ((member file-ext '("sh" "bash")) "sh")
                                                    ((member file-ext '("c")) "C")
                                                    ((member file-ext '("cpp" "cc" "cxx")) "C++")
                                                    ((member file-ext '("js")) "javascript")
                                                    ((member file-ext '("java")) "java")
                                                    ((member file-ext '("css")) "css")
                                                    ((member file-ext '("html" "htm")) "html")
                                                    ((member file-ext '("org")) "org")
                                                    ((member file-ext '("tex")) "latex"))))
                                      lang))))
    
    ;; Handle :custom block type
    (when (string= block-type ":custom")
      (setq block-type (format "\"%s\"" 
                              (read-string "Enter custom block name (with leading :): "))))
    
    ;; Insert the directive
    (insert (cond
            ((and (string= block-type "src") language)
             (format "#+INCLUDE: \"%s\" %s %s\n" smart-path block-type language))
            (t
             (format "#+INCLUDE: \"%s\" %s\n" smart-path block-type))))
    
    (message "Inserted #+INCLUDE for %s as %s block" 
             (file-name-nondirectory target-file)
             (if (string= block-type "src") 
                 (format "%s %s" block-type language)
               block-type))
    
    ;; Refresh if mode is enabled
    (when org-include-inline-mode
      (org-include-inline-refresh-buffer))))

(defun org-include-inline--get-named-blocks (file)
  "Scan FILE for named blocks and return an alist of (name . properties).
Each property list contains :name, :type (src, example, etc), and :language (for src blocks)."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((blocks nil)
          (ast (org-element-parse-buffer)))
      ;; First pass: collect all NAME keywords
      (org-element-map ast '(src-block example-block)
        (lambda (element)
          (let ((name (org-element-property :name element)))
            (when name
              (push (list name
                         :type (if (eq (org-element-type element) 'src-block)
                                 "src"
                               "example")
                         :language (org-element-property :language element))
                    blocks)))))
      (nreverse blocks))))

(defun org-include-inline-insert-named-block ()
  "Interactively select and include a named block from an Org file."
  (interactive)
  (let* ((target-file (read-file-name "Include named block from Org file: " nil nil t ".org"))
         (smart-path nil))
    
    ;; Validate file exists and is org
    (unless (and (file-exists-p target-file)
                 (string-match-p "\\.org$" target-file))
      (message "File must be an existing .org file: %s" target-file)
      (error "Invalid file"))
    
    ;; Get named blocks
    (let ((blocks (org-include-inline--get-named-blocks target-file)))
      (unless blocks
        (message "No named blocks found in %s" target-file)
        (error "No named blocks"))
      
      ;; Get the smart path
      (setq smart-path (org-include-inline--get-smart-path target-file))
      
      ;; Let user select a block
      (let* ((choices (mapcar (lambda (block)
                               (let ((name (car block))
                                     (type (plist-get (cdr block) :type))
                                     (lang (plist-get (cdr block) :language)))
                                 (format "%s (%s%s)"
                                         name
                                         type
                                         (if lang (format " - %s" lang) ""))))
                             blocks))
             (choice (completing-read "Select block: " choices nil t))
             (selected-block (nth (cl-position choice choices :test #'string=) blocks))
             (block-name (car selected-block)))
        
        ;; Insert the directive
        (insert (format "#+INCLUDE: \"%s::%s\"\n" smart-path block-name))
        (message "Inserted #+INCLUDE for named block \"%s\" from %s" 
                 block-name
                 (file-name-nondirectory target-file))
        
        ;; Refresh if mode is enabled
        (when org-include-inline-mode
          (org-include-inline-refresh-buffer))))))

;;  (defun org-include-inline-insert-from-headline ()
;;   "Interactively select an Org file and a headline to create an #+INCLUDE directive."
;;   (interactive)
;;   (let* ((current-org-buffer (current-buffer))
;;          (target-org-file (read-file-name "Include headline from Org file: " nil nil t ".org")))
;;     (unless (file-exists-p target-org-file)
;;       (message "Org file does not exist: %s" target-org-file)
;;       (error "File not found"))

;;     (let ((smart-path (org-include-inline--get-smart-path target-org-file))
;;           (headlines '()))
;;       (with-temp-buffer
;;         (insert-file-contents target-org-file)
;;         (org-mode)
;;         (org-element-map (org-element-parse-buffer 'greater-element) 'headline
;;           (lambda (h)
;;             (let* ((title (org-element-property :title h))
;;                    (custom-id (org-element-property :CUSTOM_ID h))
;;                    (level (org-element-property :level h))
;;                    (display-title (format "%s %s%s"
;;                                          (make-string level ?*) 
;;                                          (if custom-id (format "[#%s] " custom-id) "")
;;                                          (if (stringp title) title 
;;                                            (prin1-to-string title)))))
;;               (push (list display-title custom-id title level) headlines)))
;;           nil nil t)) ; include affiliated keywords
;;       (setq headlines (nreverse headlines))

;;       (unless headlines
;;         (message "No headlines found in %s" target-org-file)
;;         (error "No headlines found"))

;;       (let* ((choices (mapcar #'car headlines))
;;              (chosen-display (completing-read "Select headline: " choices nil t)))
;;         (when chosen-display
;;           (let* ((selection (cl-find chosen-display headlines 
;;                                      :key #'car :test #'string=))
;;                  (custom-id (nth 1 selection))
;;                  (title (nth 2 selection)) 
;;                  (level (nth 3 selection))
;;                  (include-spec (if custom-id
;;                                    (format "#%s" custom-id)
;;                                  (format "%s %s" 
;;                                          (make-string level ?*) 
;;                                          (if (stringp title) title (prin1-to-string title))))))
;;             (with-current-buffer current-org-buffer
;;               (insert (format "#+INCLUDE: \"%s::%s\"\n"
;;                               smart-path include-spec))
;;               (message "Inserted #+INCLUDE for headline \"%s\" from %s" 
;;                        chosen-display
;;                        smart-path)
;;               (when org-include-inline-mode
;;                 (org-include-inline-refresh-buffer))))))))) 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minor Mode Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Automatically enable org-include-inline-mode in org-mode
(defun org-include-inline-maybe-enable ()
  "Enable org-include-inline-mode if auto-enable is set."
  (when (and (derived-mode-p 'org-mode)
             org-include-inline-auto-enable-in-org-mode)
    (org-include-inline-mode 1)))
(add-hook 'org-mode-hook #'org-include-inline-maybe-enable)

;;;###autoload
(define-minor-mode org-include-inline-mode
  "Toggle display of #+INCLUDE contents inline in Org buffers.
With no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix argument disables it.

When enabled, #+INCLUDE directives will have their content displayed
inline using overlays.

Available commands:
  `org-include-inline-refresh-buffer'       - Refresh all inline includes in the current buffer
  `org-include-inline-toggle-visibility'    - Toggle the visibility of inline content
  `org-include-inline-insert-file'          - Insert a directive to include an entire file
  `org-include-inline-insert-from-lines'    - Insert a directive to include specific lines from a file
  `org-include-inline-insert-as-block'      - Insert a directive to include file as a block (src, example, etc.)
  `org-include-inline-insert-named-block'   - Insert a directive to include a named block from an Org file"
  :init-value nil
  :lighter " IInc"
  :keymap (let ((map (make-sparse-keymap)))
            map)
  :group 'org-include-inline
  (if org-include-inline-mode
      (progn
        (message "org-include-inline-mode: Enabled. Initial refresh call.")
        ;; 只在 org buffer 中添加刷新 hook
        (when (derived-mode-p 'org-mode)
          (add-hook 'after-save-hook #'org-include-inline-refresh-buffer nil t))
        ;; 在所有 buffer 中添加依赖刷新 hook
        (add-hook 'after-save-hook #'org-include-inline--refresh-dependent-buffers nil t)
        (org-include-inline-refresh-buffer))
    (progn
      (message "org-include-inline-mode: Disabled. Clearing overlays and hooks.")
      (remove-hook 'after-save-hook #'org-include-inline-refresh-buffer t)
      (remove-hook 'after-save-hook #'org-include-inline--refresh-dependent-buffers t)
      (org-include-inline--unregister-buffer (current-buffer))  ; 清理注册信息
      (org-include-inline--clear-overlays))))



(provide 'org-include-inline)
;;; org-include-inline.el ends here
