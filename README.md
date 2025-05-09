# org-include-inline

A minor mode for Org mode that displays #+INCLUDE directive contents inline within your Org buffers.

## Overview

org-include-inline enhances the Org mode editing experience by showing included content directly beneath #+INCLUDE directives, without modifying the actual buffer content. This provides immediate visual feedback while maintaining the original document structure.

![Inline Include Demo](images/figure1.gif)

## Features

- **Live Preview**: See included content directly in your buffer
- **Multiple Include Types**:
  - Include entire files
  - Include specific line ranges
- **Interactive Creation**: Easy-to-use commands for creating include directives
- **Toggle Visibility**: Show/hide included content with a single command
- **Auto-refresh**: 
  - Content updates automatically when source files change
  - Intelligent dependency tracking ensures all dependent org buffers are refreshed
  - Efficient cleanup on buffer/mode deactivation

## Installation

You can install org-include-inline through your preferred package manager. For example, with `use-package`:

```elisp
(use-package org-include-inline
  :hook (org-mode . org-include-inline-mode))
```

## Usage

### Basic Usage

1. Enable the mode in any Org buffer:
```elisp
M-x org-include-inline-mode
```

2. Create include directives using any of these commands:
- `M-x org-include-inline-insert-file` - Include an entire file
- `M-x org-include-inline-insert-from-lines` - Include specific lines from a file

3. Auto-refresh after modified the source file:
- `C-c '` go to the source file.
- Modify the source file.
- Save the source file(⚠️Must save the file with command, like `C-x C-s`, or `M-x save-buffer`).
- The included content will be updated automatically.

![Interactive Line Selection](images/figure2.gif)

### Include Directive Examples

```org
# Include an entire file
#+INCLUDE: "path/to/file.org"

# Include specific lines
#+INCLUDE: "path/to/file.org" :lines "5-10"
```

## Commands

- `org-include-inline-refresh-buffer` - Refresh all inline includes in the current buffer
- `org-include-inline-toggle-visibility` - Toggle visibility of all inline content
- `org-include-inline-insert-file` - Insert a directive to include an entire file
- `org-include-inline-insert-from-lines` - Insert a directive to include specific lines

## Customization

```elisp
;; Auto-enable in all Org buffers
(setq org-include-inline-auto-enable-in-org-mode t)

;; Customize maximum lines to display
(setq org-include-inline-max-lines-to-display 1000)

;; Customize the display face
(set-face-attribute 'org-include-inline-face nil
                    :background "black"
                    :foreground "white")
```

## Comparison with org-transclusion

While both org-include-inline and org-transclusion deal with including content from other files, they serve different purposes:

- **Purpose**:
  - org-include-inline: Focuses on visualizing Org's native #+INCLUDE directives inline
  - org-transclusion: Provides a more general transclusion system for various content types

- **Implementation**:
  - org-include-inline: Uses overlays to display content beneath #+INCLUDE lines
  - org-transclusion: Creates actual text content in the buffer that can be edited

- **Use Cases**:
  - org-include-inline: Best for:
    - Working with existing #+INCLUDE directives
    - Quick preview of included content
    - Source code inclusion and documentation
  - org-transclusion: Better for:
    - Complex transclusion needs
    - Live editing of transcluded content
    - Advanced linking between documents

- **Simplicity**:
  - org-include-inline: Lightweight, focused on one specific feature
  - org-transclusion: More feature-rich but with higher complexity

Choose org-include-inline if you mainly work with #+INCLUDE directives and want a simple, visual way to see included content. Choose org-transclusion for more advanced document transclusion needs.

## Contributing

Contributions are welcome! Feel free to:
- Report issues
- Suggest enhancements
- Submit pull requests

## License

This project is licensed under the GNU General Public License v3.0.

## Author

Yibie (gunshotbox@gmail.com)