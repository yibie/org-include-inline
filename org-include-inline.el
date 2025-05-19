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
(require 'org-id)

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

(defcustom org-include-inline-auto-save t
  "Whether to automatically save buffers after refreshing includes.
When non-nil, buffers will be saved after their includes are refreshed
to ensure the dependency relationships are persisted."
  :type 'boolean
  :group 'org-include-inline)

(defcustom org-include-inline-auto-refresh-key "C-c C-x C-v"
  "Key binding for auto-refresh command in org-include-inline-mode."
  :type 'string
  :group 'org-include-inline)

(defcustom org-include-inline-additional-id-formats nil
  "Additional regular expressions to recognize org IDs.
Each entry should be a regular expression string that matches your custom ID format.
For example: '(\"\\`[A-Z]+[0-9]+\\'\") to match IDs like 'ABC123'."
  :type '(repeat string)
  :group 'org-include-inline)

(defcustom org-include-inline-respect-folding t
  "Whether to hide includes when their parent heading is folded."
  :type 'boolean
  :group 'org-include-inline)

(defcustom org-include-inline-export-behavior 'selective
  "How to handle includes during export.
Possible values:
- selective: process includes normally, except those marked with :export: no
- ignore: completely ignore all includes
- process: process all includes normally (same as org default)"
  :type '(choice
          (const :tag "Selective processing (default)" selective)
          (const :tag "Ignore all includes" ignore)
          (const :tag "Process all includes" process))
  :group 'org-include-inline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal Variables and Data Structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local org-include-inline--overlays nil
  "Buffer-local list of overlays created by org-include-inline-mode.")

(defvar org-include-inline--source-buffers nil
  "Alist of (source-file . org-buffers) pairs.
Each pair maps a source file to a list of org buffers that include it.")

(defvar org-include-inline--storage-file
  (expand-file-name "org-include-inline-storage.el" user-emacs-directory)
  "File to store org-include-inline associations.")

(defvar org-include-inline--block-types
  '("src" "example" "export" ":custom")
  "List of common block types for #+INCLUDE directives.")

(defvar org-include-inline--common-languages
  '("emacs-lisp" "python" "sh" "C" "C++" "java" "javascript" "css" "html" "org" "latex")
  "List of common programming languages for src blocks.")

(defvar-local org-include-inline--refreshing nil
  "Flag to prevent recursive refresh.")

(defvar org-include-inline--last-refresh-time (make-hash-table :test 'equal)
  "Hash table to store last refresh time for each buffer.")

(defvar-local org-include-inline--original-includes nil
  "Alist to store original includes during export process.
Each element is a cons cell (keyword . value) where keyword is the org-element
and value is the original include directive value.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-include-inline--save-associations ()
  "Save current associations to storage file."
  (let ((data-to-save
         (mapcar (lambda (entry)
                   (cons (car entry) ; source-file path (string)
                         ;; Convert buffer objects to their file paths
                         (mapcar #'buffer-file-name
                                (cl-remove-if-not
                                 (lambda (b)
                                   (and (bufferp b)
                                        (buffer-live-p b)
                                        (buffer-file-name b)))
                                 (cdr entry)))))
                 org-include-inline--source-buffers)))
    ;; Filter out entries where the list of buffer files became empty
    (setq data-to-save (cl-remove-if (lambda (entry) (null (cdr entry))) data-to-save))
    (when data-to-save
      (let ((inhibit-message t)) ;; 抑制保存消息
        (with-temp-file org-include-inline--storage-file
          (insert ";; -*- mode: emacs-lisp; lexical-binding: t; -*-\n")
          (insert ";; Org Include Inline Storage\n")
          (insert ";; This file is auto-generated. Do not edit by hand.\n\n")
          (insert "(setq org-include-inline--source-buffers\n")
          (insert (format "  '%S)\n" data-to-save)))))))

(defun org-include-inline--load-associations ()
  "Load associations from storage file."
  (when (file-exists-p org-include-inline--storage-file)
    (load org-include-inline--storage-file)
    ;; 将文件路径转换为 buffer
    (setq org-include-inline--source-buffers
          (mapcar (lambda (entry)
                    (cons (car entry)  ; source file path
                          ;; Convert file paths to buffers, only if they exist
                          (mapcar (lambda (file)
                                   (or (find-buffer-visiting file)
                                       (when (file-exists-p file)
                                         (find-file-noselect file))))
                                 (cdr entry))))
                  org-include-inline--source-buffers))
    ;; Remove any entries where all buffers failed to load
    (setq org-include-inline--source-buffers
          (cl-remove-if (lambda (entry) (null (cdr entry)))
                        org-include-inline--source-buffers))))

(defun org-include-inline--parse-include-directive (line-text)
  "Parse an #+INCLUDE line and return a plist with info.
The line should be in format: 
#+INCLUDE: \"FILE\" [BLOCK-TYPE LANGUAGE] [:lines \"N-M\"] or
#+INCLUDE: \"FILE::*HEADLINE\" or
#+INCLUDE: \"FILE::BLOCK-NAME\" or
#+INCLUDE: \"FILE::ID\"

BLOCK-TYPE can be 'src', 'example', etc.
LANGUAGE is the source code language when BLOCK-TYPE is 'src' or 'export'."
  (let ((case-fold-search t)
        file type lines-spec headline-spec block-type language named-block id-spec
        line-after-file remainder-after-headline)

    ;; 1. Match #+INCLUDE: "FILE" part and get the rest of the line
    (if (string-match "^[ \t]*#\\+INCLUDE:[ \t]*\"\\([^\"]+\\)\"\\(.*\\)" line-text)
        (progn
          (setq file (match-string 1 line-text))
          (setq line-after-file (match-string 2 line-text))

          ;; Check if this is a named block reference (file::block-name) or headline (file::*headline) or ID
          (when (string-match "\\(.*\\)::\\([^:]+\\)\\'" file)
            (let ((main-file (match-string 1 file))
                  (spec (match-string 2 file)))
              (cond
               ;; ID format
               ((org-include-inline--is-valid-org-id-p spec)
                (setq type :id
                      id-spec spec
                      file main-file))
               ;; Headline format
               ((or (string-prefix-p "*" spec)
                    (string-prefix-p "#" spec))
                (setq headline-spec spec
                      file main-file
                      type :headline))
               ;; Named block
               (t
                (setq named-block spec
                      file main-file
                      type :named-block)))
              ))

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
             (headline-spec (setq type :headline))
             (named-block (setq type :named-block))
             ((and (not type) lines-spec) (setq type :lines))
             ((not type) (setq type :lines) (setq lines-spec "1-"))))

          `(:file ,file 
            :type ,type
            :lines-spec ,lines-spec
            :headline-spec ,headline-spec
            :block-type ,block-type
            :language ,language
            :named-block ,named-block
            :id-spec ,id-spec
            :original-line ,line-text))
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
  
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let* ((ast (org-element-parse-buffer))
           (block (org-element-map ast '(src-block example-block)
                   (lambda (element)
                     (when (string= (org-element-property :name element) block-name)
                       element))
                   nil t)))
      (if block
          (let* ((content (org-element-property :value block)))
            (if content
                (let ((trimmed (string-trim content)))
                  trimmed)
              "")) ; Return empty string if no content
        (format "Error: Named block \"%s\" not found in %s" 
                block-name (file-name-nondirectory file))))))

(defun org-include-inline--fetch-org-headline-content (file headline-spec &optional only-contents lines-spec)
  "Fetch content of a specific headline from an Org FILE.
HEADLINE-SPEC 可以是 \"*Headline Text\" 或 \"#custom-id\"。
If ONLY-CONTENTS is non-nil, return only the content (excluding property drawers, planning, etc.).
LINES-SPEC is like \"1-10\", returning only a portion of the content."
  (unless (file-readable-p file)
    (message "Error: Org file not readable: %s" file)
    (return-from org-include-inline--fetch-org-headline-content (format "Error: Org file not readable: %s" file)))
  (unless headline-spec
    (message "Error: No headline specification provided for file %s" file)
    (return-from org-include-inline--fetch-org-headline-content (format "Error: No headline specified for %s" file)))
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let* ((ast (org-element-parse-buffer 'greater-element))
           (target-headline
            (org-element-map ast 'headline
              (lambda (headline)
                (let ((title (org-element-property :raw-value headline))
                      (custom-id (org-element-property :CUSTOM_ID headline)))
                  (cond
                   ((and (string-prefix-p "#" headline-spec)
                         custom-id
                         (string= custom-id (substring headline-spec 1)))
                    headline)
                   ((and (string-prefix-p "*" headline-spec)
                         title
                         (string= (string-trim (replace-regexp-in-string "^\\*+\\s-*" "" headline-spec))
                                  (string-trim title)))
                    headline))))
              nil t)))
      (if (not target-headline)
          (format "Error: Headline/target '%s' not found in %s" headline-spec (file-name-nondirectory file))
        (let* ((content-begin (org-element-property :contents-begin target-headline))
               (content-end (org-element-property :contents-end target-headline))
               (raw-content (if (and content-begin content-end)
                                (buffer-substring-no-properties content-begin content-end)
                              ""))
               (final-content raw-content))
          (when only-contents
            (with-temp-buffer
              (insert raw-content)
              (goto-char (point-min))
              (when (re-search-forward "^:PROPERTIES:$" nil t)
                (let ((prop-end (and (re-search-forward "^:END:$" nil t) (point))))
                  (when prop-end (delete-region (point-min) prop-end))))
              (goto-char (point-min))
              (while (looking-at "^\s-*\(DEADLINE:\|SCHEDULED:\|CLOSED:\)")
                (forward-line 1))
              (setq final-content (buffer-substring-no-properties (point) (point-max)))))
          (when (and lines-spec (not (string-empty-p final-content)))
            (let* ((lines (split-string final-content "\n"))
                   (start 1)
                   (end (length lines)))
              (cond
               ((string-match "\\`\\([0-9]+\\)-\\([0-9]+\\)\\'" lines-spec)
                (setq start (string-to-number (match-string 1 lines-spec)))
                (setq end (string-to-number (match-string 2 lines-spec))))
               ((string-match "\\`\\([0-9]+\\)-\\'" lines-spec)
                (setq start (string-to-number (match-string 1 lines-spec))))
               ((string-match "\\`-\\([0-9]+\\)\\'" lines-spec)
                (setq end (string-to-number (match-string 1 lines-spec))))
               ((string-match "\\`\\([0-9]+\\)\\'" lines-spec)
                (setq start (string-to-number (match-string 1 lines-spec)))
                (setq end start)))
              (setq start (max 1 start))
              (setq end (min (length lines) end))
              (setq final-content (mapconcat #'identity (cl-subseq lines (1- start) end) "\n"))))
          (string-trim final-content))))))

(defun org-include-inline--fetch-org-id-content (id &optional only-contents lines-spec)
  "Fetch content of an entry with ID.
ID is the entry ID (UUID, internal format, or custom format).
If ONLY-CONTENTS is non-nil, return only the contents (excluding properties).
LINES-SPEC is like \"1-10\", returning only those lines."
  (require 'org-id)
  (if-let ((marker (org-id-find id t)))
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (beginning-of-line)
          (let ((content
                 (save-restriction
                   (org-narrow-to-subtree)
                   (let ((raw-content (buffer-substring-no-properties (point) (point-max))))
                     (with-temp-buffer
                       (org-mode)
                       (insert raw-content)
                       (goto-char (point-min))
                       (forward-line 1) 
                       (while (looking-at org-drawer-regexp)
                         (let ((drawer-end (save-excursion
                                           (re-search-forward "^[ \t]*:END:[ \t]*$" nil t))))
                           (when drawer-end
                             (delete-region (point) (progn (goto-char drawer-end) 
                                                         (forward-line 1)
                                                         (point))))))
                       (goto-char (point-min))
                       (forward-line 1)  
                       (while (looking-at org-planning-line-re)
                         (delete-region (point) (progn (forward-line 1) (point))))
                       (buffer-substring-no-properties (point-min) (point-max)))))))
            (when (and content lines-spec)
              (let* ((lines (split-string content "\n"))
                     (start 1)
                     (end (length lines)))
                (when (string-match "\\`\\([0-9]+\\)-\\([0-9]+\\)\\'" lines-spec)
                  (setq start (string-to-number (match-string 1 lines-spec))
                        end (string-to-number (match-string 2 lines-spec)))
                  (setq start (max 1 start)
                        end (min (length lines) end))
                  (setq content (mapconcat #'identity 
                                         (cl-subseq lines (1- start) end)
                                         "\n")))))
            (let ((final-content (if content (string-trim content) "")))
              (or (and final-content (not (string-empty-p final-content))
                      final-content)
                  (format "Error: No content found for ID %s" id))))))
    (format "Error: Entry with ID %s not found" id)))

(defun org-include-inline--create-or-update-overlay (point content &optional buffer)
  "Create or update an overlay at POINT to display CONTENT in BUFFER.
If BUFFER is nil, use current buffer. Ensures overlay stays attached to buffer."
  (let ((buf (or buffer (current-buffer))))
    (if (and content (> (length content) 0))
        (progn
          (condition-case e-make-overlay
              (with-current-buffer buf
                (let ((ov (make-overlay point point buf))
                      (parent-pos (save-excursion
                                  (condition-case nil
                                      (progn
                                        (org-back-to-heading t)
                                        (point))
                                    (error nil)))))
                  ;; Store overlay's parent heading position if exists
                  (overlay-put ov 'org-include-inline t)
                  (when parent-pos
                    (overlay-put ov 'org-include-parent-heading parent-pos))
                  ;; Use before-string for better visibility control
                  (overlay-put ov 'before-string 
                             (propertize content 
                                       'org-include-inline t
                                       'invisible 'org-include-inline))
                  (overlay-put ov 'evaporate nil)
                  (overlay-put ov 'priority 100)
                  (push ov org-include-inline--overlays)))
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
  (when (and (boundp 'org-include-inline-mode) org-include-inline-mode)
    (remhash (current-buffer) org-include-inline--last-refresh-time)
    (org-include-inline--unregister-buffer (current-buffer))))

(add-hook 'kill-buffer-hook #'org-include-inline--cleanup-on-kill)

(defun org-include-inline--register-source-file (source-file org-buffer)
  "Register that ORG-BUFFER (a buffer object) includes SOURCE-FILE (a path string)."
  (let* ((source-path (expand-file-name source-file))
         (existing-entry (assoc source-path org-include-inline--source-buffers)))
    (if existing-entry
        (unless (member org-buffer (cdr existing-entry))
          (setf (cdr existing-entry) (cons org-buffer (cdr existing-entry)))
        (push (cons source-path (list org-buffer))
              org-include-inline--source-buffers)
        (org-include-inline--save-associations)))))

(defun org-include-inline--unregister-buffer (buffer)
  "Remove BUFFER (a buffer object) from all source file registrations."
  (setq org-include-inline--source-buffers
        (cl-loop for (source-file . buffers) in org-include-inline--source-buffers
                 ;; `buffers` is a list of buffer objects after load_associations
                 for new-buffers = (remq buffer buffers) ; Remove the specific buffer object
                 when new-buffers
                 collect (cons source-file new-buffers)
                 ;; If new-buffers is empty, this source-file entry will be filtered out by save-associations
                 ))
  (org-include-inline--save-associations))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (unless org-include-inline--refreshing  
    (let ((org-include-inline--refreshing t))
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
                              ((eq (plist-get include-info :type) :id)
                               (org-include-inline--fetch-org-id-content
                                (plist-get include-info :id-spec)))
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
      
      (message "Refresh complete. Processed %d includes." count))))))

(defun org-include-inline--refresh-dependent-buffers ()
  "Refresh all org buffers that include the current buffer's file."
  (unless org-include-inline--refreshing  ;; 防止递归
    (when buffer-file-name
      (let ((org-include-inline--refreshing t)
            (source-path (expand-file-name buffer-file-name)))
        (dolist (buffer-entry org-include-inline--source-buffers)
          (when (string= (car buffer-entry) source-path)
            (dolist (org-buffer (cdr buffer-entry))
              (when (buffer-live-p org-buffer)
                (with-current-buffer org-buffer
                  (org-include-inline-refresh-buffer))))))))))

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
    (with-current-buffer org-buf
      (let ((actual-include-path rel-path))
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

(defun org-include-inline-insert-headline ()
  "Interactively select and include a headline from an Org file.
This allows including content from a specific headline/subtree in an Org file,
either by selecting the headline text or its CUSTOM_ID if available."
  (interactive)
  (let* ((current-org-buffer (current-buffer))
         (target-file (read-file-name "Include headline from Org file: " nil nil t ".org")))

    (unless (and (file-exists-p target-file)
                 (string-match-p "\\.org$" target-file))
      (user-error "File must be an existing .org file: %s" target-file))
    
    (let ((smart-path (org-include-inline--get-smart-path target-file))
          (headlines '()))
      
      (with-temp-buffer
        (insert-file-contents target-file)
        (org-mode)
        (org-element-map (org-element-parse-buffer 'greater-element) 'headline
          (lambda (h)
            (let* ((title (org-element-property :raw-value h))
                   (custom-id (org-element-property :CUSTOM_ID h))
                   (level (org-element-property :level h))
                   (display-title (format "%s %s%s"
                                        (make-string level ?*)
                                        (if custom-id (format "[#%s] " custom-id) "")
                                        (if (stringp title) title
                                          (prin1-to-string title)))))
              (push (list display-title custom-id title level) headlines)))
          nil nil t))
      
      (setq headlines (nreverse headlines))
      
      (unless headlines
        (user-error "No headlines found in %s" target-file))
      
      (let* ((choices (mapcar #'car headlines))
             (chosen-display (completing-read "Select headline: " choices nil t)))
        (when chosen-display
          (let* ((selection (cl-find chosen-display headlines :key #'car :test #'string=))
                 (custom-id (nth 1 selection))
                 (title (nth 2 selection))
                 (level (nth 3 selection))
                 (include-spec (if custom-id
                                 (format "#%s" custom-id)
                               (format "*%s" title))))
            
            (with-current-buffer current-org-buffer
              (insert (format "#+INCLUDE: \"%s::%s\"\n"
                            smart-path include-spec))
              (message "Inserted #+INCLUDE for headline \"%s\" from %s"
                      chosen-display
                      (file-name-nondirectory target-file))
              
              (when org-include-inline-mode
                (org-include-inline-refresh-buffer)))))))))

(defun org-include-inline--get-entries-with-ids (file)
  "Get all entries with IDs from FILE.
Returns an alist, format ((title . id) ...) where title includes hierarchy and text.
FILE is the file to search for entries with IDs."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (org-element-map (org-element-parse-buffer 'greater-element) 'headline
      (lambda (h)
        (let* ((title (org-element-property :raw-value h))
               (level (org-element-property :level h))
               (id (org-element-property :ID h)))
          (when id
            (cons (format "%s %s"
                         (make-string level ?*)
                         (if (stringp title) title
                           (prin1-to-string title)))
                  id))))
      nil nil t)))

(defun org-include-inline-insert-id ()
  "Interactively insert an #+INCLUDE directive based on ID.
Allows selecting an entry with ID from an Org file."
  (interactive)
  (let* ((current-org-buffer (current-buffer))
         (target-file (read-file-name "Select Org file containing target ID: " nil nil t ".org")))
    
    (unless (and (file-exists-p target-file)
                 (string-match-p "\\.org$" target-file))
      (user-error "File must be an existing .org file: %s" target-file))
    
    (let* ((smart-path (org-include-inline--get-smart-path target-file))
           (entries (org-include-inline--get-entries-with-ids target-file)))
      
      (unless entries
        (user-error "No entries with IDs found in %s" target-file))
      
      (let* ((choices (mapcar (lambda (entry)
                               (format "%s [%s]" (car entry) (cdr entry)))
                             entries))
             (chosen (completing-read "Select entry to include: " choices nil t))
             (chosen-id (cdr (nth (cl-position chosen choices :test #'string=)
                                 entries))))
        
        (with-current-buffer current-org-buffer
          (insert (format "#+INCLUDE: \"%s::%s\"\n"
                         smart-path chosen-id))
          (message "Inserted #+INCLUDE directive for ID %s" chosen-id)
          
          (when org-include-inline-mode
            (org-include-inline-refresh-buffer)))))))

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

(defun org-include-inline--after-save-handler ()
  "Handle after-save-hook for source files."
  (when (and buffer-file-name
             (not org-include-inline--refreshing))  
    (let ((org-include-inline--refreshing t)
          (source-path (expand-file-name buffer-file-name)))
      (when (assoc source-path org-include-inline--source-buffers)
        (dolist (buffer (cdr (assoc source-path org-include-inline--source-buffers)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (org-include-inline-refresh-buffer)
              (puthash buffer (float-time) org-include-inline--last-refresh-time)))))
      
      (when (and org-include-inline-mode
                 (derived-mode-p 'org-mode))
        (org-include-inline-refresh-buffer)
        (puthash (current-buffer) (float-time) org-include-inline--last-refresh-time)))))

(defun org-include-inline--setup-file-watch ()
  "Set up file watching for source files."
  (dolist (entry org-include-inline--source-buffers)
    (let ((source-file (car entry)))
      (when (and (stringp source-file)
                 (file-exists-p source-file))
        (file-notify-add-watch
         source-file
         '(change)
         (lambda (event)
           (when (eq (nth 1 event) 'changed)
             (let ((source-path (nth 2 event)))
               (dolist (buffer (cdr (assoc source-path org-include-inline--source-buffers)))
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (org-include-inline-refresh-buffer)
                     (puthash buffer (float-time) org-include-inline--last-refresh-time))))))))))))

(defun org-include-inline--is-valid-org-id-p (id)
  "Check if ID is a valid Org ID string.
Supports various ID formats including UUID, org's internal format, and timestamp-based IDs.
Also supports custom formats defined in `org-include-inline-additional-id-formats'."
  (and (stringp id)
       (or
        ;; UUID format: 8-4-4-4-12 hex digits
        (string-match-p "\\`[A-Fa-f0-9]\\{8\\}-[A-Fa-f0-9]\\{4\\}-[A-Fa-f0-9]\\{4\\}-[A-Fa-f0-9]\\{4\\}-[A-Fa-f0-9]\\{12\\}\\'" id)
        ;; Org's internal format: 32 hex digits
        (string-match-p "\\`[A-Fa-f0-9]\\{32\\}\\'" id)
        ;; Timestamp format: YYYY-MM-DD-HH-MM-SS.XXXXXX_XXXXX
        (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.[0-9]+_[A-Za-z0-9]+\\'" id)
        ;; Check against additional custom formats
        (cl-some (lambda (pattern)
                   (string-match-p pattern id))
                 org-include-inline-additional-id-formats))))

(defun org-include-inline--advice-org-link-open-as-file (old-fn path arg)
  "Advice for `org-link-open-as-file' to handle org-include-inline ID links.
If PATH contains an ID reference, find and jump to that ID.
Otherwise, call the original function OLD-FN with PATH and ARG."
  (if (and path
           (string-match "\\(.*\\)::\\([A-Za-z0-9-]+\\)\\'" path))
      (let* ((file (match-string 1 path))
             (id (match-string 2 path)))
        ;; Check if it looks like an ID
        (if (org-include-inline--is-valid-org-id-p id)
            (condition-case err
                (progn
                  ;; Open the file first
                  (find-file file)
                  ;; Then try to find the ID
                  (or (org-id-goto id)
                      (user-error "Cannot find ID \"%s\" in file %s" 
                                id (file-name-nondirectory file))))
              (error
               (message "Error jumping to ID %s: %S" id err)
               (user-error "Cannot open file %s or find ID \"%s\"" 
                           (file-name-nondirectory file) id)))
          ;; Not an ID, call original function
          (funcall old-fn path arg)))
    ;; No ID pattern, call original function
    (funcall old-fn path arg)))

;; Remove old advice if it exists
(when (advice-member-p #'org-include-inline--advice-org-open-at-point 'org-open-at-point)
  (advice-remove 'org-open-at-point #'org-include-inline--advice-org-open-at-point))

;; Add new advice
(unless (advice-member-p #'org-include-inline--advice-org-link-open-as-file 'org-link-open-as-file)
  (advice-add 'org-link-open-as-file :around #'org-include-inline--advice-org-link-open-as-file))

(defun org-include-inline--update-folding-state (&optional cycle-state)
  "Update visibility of includes based on heading fold state.
CYCLE-STATE is the folding state passed by `org-cycle-hook', but we don't use it
directly as we check each heading's actual folded state individually."
  (when org-include-inline-respect-folding
    (save-excursion
      (dolist (ov org-include-inline--overlays)
        (when (overlay-buffer ov)  ; ensure overlay still exists
          (let* ((parent-pos (overlay-get ov 'org-include-parent-heading))
                 (before-str (overlay-get ov 'before-string)))
            ;; If no parent heading, always show the include
            (if (not parent-pos)
                (when before-str
                  (put-text-property 0 (length before-str) 'invisible nil before-str))
              ;; Otherwise check parent heading's fold state
              (goto-char parent-pos)
              (let ((folded (org-fold-folded-p)))
                (when before-str
                  (put-text-property 0 (length before-str) 'invisible 
                                   (when folded 'org-include-inline) 
                                   before-str))))))))))

(defun org-include-inline--export-filter (backend)
  "Filter function for export process.
Handles includes according to `org-include-inline-export-behavior':
- selective: process includes normally, except those marked with :export: no
- ignore: completely ignore all includes
- process: process all includes normally (same as org default)"
  (when (bound-and-true-p org-include-inline-mode)
    (let ((includes (org-element-map (org-element-parse-buffer) 'keyword
                     (lambda (keyword)
                       (when (string= (org-element-property :key keyword) "INCLUDE")
                         keyword)))))
      ;; Save original state
      (setq-local org-include-inline--original-includes
                  (mapcar (lambda (inc)
                           (cons inc (org-element-property :value inc)))
                          includes))
      
      (pcase org-include-inline-export-behavior
        ('ignore
         ;; Comment out all includes
         (dolist (inc includes)
           (goto-char (org-element-property :begin inc))
           (insert "# ")))
        
        ('selective
         ;; Only comment out includes explicitly marked with :export: no
         (dolist (inc includes)
           (save-excursion
             (goto-char (org-element-property :begin inc))
             ;; Check for :export: property in the line
             (let* ((line (buffer-substring-no-properties 
                          (line-beginning-position) 
                          (line-end-position)))
                    (export-prop (when (string-match ":export:\\s-*\\([^ \t\n]+\\)" line)
                                 (match-string 1 line))))
               ;; Comment out only if :export: is explicitly "no"
               (when (string= export-prop "no")
                 (goto-char (line-beginning-position))
                 (insert "# "))))))
        
        ('process
         ;; Do nothing, let org process all includes normally
         nil)))))

(defun org-include-inline--after-export ()
  "Restore includes after export."
  (when (bound-and-true-p org-include-inline--original-includes)
    (save-excursion
      (dolist (pair org-include-inline--original-includes)
        (let ((keyword (car pair)))
          (goto-char (org-element-property :begin keyword))
          (when (looking-at "^#\\s-*")  ; Only remove if we added the comment
            (delete-char 2)))))
    (setq-local org-include-inline--original-includes nil)))

;;;###autoload
(define-minor-mode org-include-inline-mode
  "Toggle display of #+INCLUDE contents inline in Org buffers.
With no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix argument disables it.

When enabled, #+INCLUDE directives will have their content displayed
inline using overlays.

Available commands:
  `org-include-inline-refresh-buffer'       - Refresh all inline includes in the current buffer
  `org-include-inline-insert-file'          - Insert a directive to include an entire file
  `org-include-inline-insert-from-lines'    - Insert a directive to include specific lines from a file
  `org-include-inline-insert-as-block'      - Insert a directive to include file as a block (src, example, etc.)
  `org-include-inline-insert-named-block'   - Insert a directive to include a named block from an Org file
  `org-include-inline-insert-headline'      - Insert a directive to include a headline/subtree from an Org file
  `org-include-inline-insert-id'            - Insert a directive to include an entry with ID"
  :init-value nil
  :lighter " IInc"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd org-include-inline-auto-refresh-key) 
                        #'org-include-inline-refresh-buffer)
            map)
  :group 'org-include-inline
  (if org-include-inline-mode
      (progn
        (message "Enabling org-include-inline-mode in %s" (buffer-name))
        (unless org-include-inline--source-buffers
          (org-include-inline--load-associations))
        ;; Add export hooks
        (add-hook 'org-export-before-processing-hook 
                  #'org-include-inline--export-filter nil t)
        (add-hook 'org-export-after-processing-hook 
                  #'org-include-inline--after-export nil t)
        ;; Add folding hook
        (add-hook 'org-cycle-hook 
                  #'org-include-inline--update-folding-state nil t)
        (add-hook 'after-save-hook #'org-include-inline--after-save-handler nil t)
        (add-hook 'after-revert-hook #'org-include-inline-refresh-buffer nil t)
        (add-hook 'window-configuration-change-hook #'org-include-inline-refresh-buffer nil t)
        (let ((org-include-inline--refreshing t))
          (org-include-inline-refresh-buffer)
          (puthash (current-buffer) (float-time) org-include-inline--last-refresh-time)))
    (progn
      (message "Disabling org-include-inline-mode in %s" (buffer-name))
      ;; Remove export hooks
      (remove-hook 'org-export-before-processing-hook 
                   #'org-include-inline--export-filter t)
      (remove-hook 'org-export-after-processing-hook 
                   #'org-include-inline--after-export t)
      ;; Remove folding hook
      (remove-hook 'org-cycle-hook 
                   #'org-include-inline--update-folding-state t)
      (remove-hook 'after-save-hook #'org-include-inline--after-save-handler t)
      (remove-hook 'after-revert-hook #'org-include-inline-refresh-buffer t)
      (remove-hook 'window-configuration-change-hook #'org-include-inline-refresh-buffer t)
      (remhash (current-buffer) org-include-inline--last-refresh-time)
      (org-include-inline--unregister-buffer (current-buffer))
      (org-include-inline--clear-overlays))))

(defalias 'org-include-inline-refresh 'org-include-inline-refresh-buffer)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-x C-v") #'org-include-inline-refresh-buffer))

(provide 'org-include-inline)
;;; org-include-inline.el ends here
