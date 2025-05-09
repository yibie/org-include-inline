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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-include-inline--parse-include-directive (line-text)
  "Parse an #+INCLUDE line and return a plist with info.
The line should be in format: #+INCLUDE: \"FILE\" [:lines \"N-M\"] or
#+INCLUDE: \"FILE::HEADLINE\"."
  (let ((case-fold-search t)
        file type lines-spec headline-spec
        line-after-file remainder-after-headline)

    ;; 1. Match #+INCLUDE: "FILE" part and get the rest of the line
    (if (string-match "^[ \t]*#\\+INCLUDE:[ \t]*\"\\([^\"]+\\)\"\\(.*\\)" line-text)
        (progn
          (setq file (match-string 1 line-text))
          (setq line-after-file (match-string 2 line-text))

          (when file
            (setq file (expand-file-name file (if buffer-file-name
                                                 (file-name-directory buffer-file-name)
                                               default-directory)))
            ;; Initially, the remainder for further parsing is everything after the file part
            (setq remainder-after-headline line-after-file)

            ;; 2. Try to parse ::headline-spec from line-after-file
            (when (string-match "^[ \t]*::\\(.+?\\)[ \t]*\\(.*\\)" line-after-file)
              (setq headline-spec (string-trim (match-string 1 line-after-file)))
              (setq remainder-after-headline (match-string 2 line-after-file)))

            ;; 3. Try to parse :lines from remainder-after-headline
            (when (string-match "^[ \t]*:lines[ \t]+\"\\([0-9]+-\?[0-9]*\\)\"" remainder-after-headline)
              (setq lines-spec (match-string 1 remainder-after-headline)))

            ;; Determine type
            (cond
             (headline-spec (setq type :headline))
             (lines-spec (setq type :lines))
             (t (setq type :lines) (setq lines-spec "1-")))

            `(:file ,file :type ,type :lines-spec ,lines-spec :headline-spec ,headline-spec :original-line ,line-text)))
      nil)))


(defun org-include-inline--fetch-file-lines (file lines-spec)
  "Fetch specific lines from FILE based on LINES-SPEC (e.g., \"1-10\", \"5\")."
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
    content))

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
  "Remove all org-include-inline overlays from the current buffer. (Verbose Debugging)"
  (message "org-include-inline--clear-overlays: ===ENTERED=== Buffer: %s. Current org-include-inline--overlays list (before clearing): %d items, list: %S"
           (current-buffer)
           (if (boundp 'org-include-inline--overlays) (length org-include-inline--overlays) 0)
           org-include-inline--overlays)
  (sit-for 0.01)
  (let ((cleared-count 0))
    (if (and (boundp 'org-include-inline--overlays) org-include-inline--overlays) ; Check if list exists and is not empty
        (dolist (ov org-include-inline--overlays)
          (when (overlayp ov) ; Make sure it's still a valid overlay
            (message "org-include-inline--clear-overlays: Deleting overlay: %S (start: %s, end: %s, buffer: %s)"
                     ov (overlay-start ov) (overlay-end ov) (overlay-buffer ov))
	    (sit-for 0.01)
            (delete-overlay ov)
            (setq cleared-count (1+ cleared-count))))
      (message "org-include-inline--clear-overlays: org-include-inline--overlays list was empty or not bound.") (sit-for 0.01))
    (setq org-include-inline--overlays nil) ; Reset the list
    (message "org-include-inline--clear-overlays: ===EXITING===. Cleared %d overlays. List is now: %S"
             cleared-count org-include-inline--overlays)
    (sit-for 0.01)))

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

(defun org-include-inline-refresh-buffer ()
  "Refresh all inline includes in the current buffer."
  (interactive)
  (message "Refreshing inline includes...")
  (org-include-inline--clear-overlays)

  (when (and (derived-mode-p 'org-mode) org-include-inline-mode)
    (let ((current-buffer (current-buffer))
          (count 0))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp "^[ \t]*#\\+INCLUDE:" nil t)
          (setq count (1+ count))
          
          (let* ((match-line-start (match-beginning 0))
                 (current-line-text
                  (buffer-substring-no-properties 
                   (line-beginning-position) (line-end-position)))
                 (include-info (org-include-inline--parse-include-directive current-line-text))
                 (content nil)
                 (overlay-pos-calculation-error nil)
                 (overlay-pos
                  (condition-case e
                      (progn
                        (forward-line 1)
                        (point))
                    ((debug error)
                     (setq overlay-pos-calculation-error t)
                     nil))))

            (when include-info
              (setq content
                    (cond
                     ((eq (plist-get include-info :type) :lines)
                      (org-include-inline--fetch-file-lines 
                       (plist-get include-info :file)
                       (plist-get include-info :lines-spec)))
                     ((eq (plist-get include-info :type) :headline)
                      (org-include-inline--fetch-org-headline-content 
                       (plist-get include-info :file)
                       (plist-get include-info :headline-spec)))
                     (t (format "Error: Unknown include type for %s" 
                              (plist-get include-info :original-line)))))

              (unless overlay-pos-calculation-error
                (when (and content overlay-pos)
                  (org-include-inline--create-or-update-overlay 
                   overlay-pos content current-buffer)))))))
      (message "Refresh complete. Processed %d includes." count))))

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
  `org-include-inline-refresh-buffer`       - Refresh all inline includes in the current buffer
  `org-include-inline-toggle-visibility`    - Toggle the visibility of inline content
  `org-include-inline-insert-file`          - Insert a directive to include an entire file
  `org-include-inline-insert-from-lines`    - Insert a directive to include specific lines from a file"
  :init-value nil
  :lighter " IInc"
  :keymap (let ((map (make-sparse-keymap)))
            ;; (define-key map (kbd "C-c C-i r") 'org-include-inline-refresh-buffer) ; Example keybinding
            map)
  :group 'org-include-inline
  (if org-include-inline-mode
      (progn
        (message "org-include-inline-mode: Enabled. Initial refresh call.")
        ;; Automatically refresh when the target file is saved
        (add-hook 'after-save-hook #'org-include-inline-refresh-buffer nil t)
        (org-include-inline-refresh-buffer))
    (progn
      (message "org-include-inline-mode: Disabled. Clearing overlays and hooks.")
      (remove-hook 'after-save-hook #'org-include-inline-refresh-buffer t)
      (org-include-inline--clear-overlays))))


(provide 'org-include-inline)
;;; org-include-inline.el ends here
