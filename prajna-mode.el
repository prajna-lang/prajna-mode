;;; prajna-mode.el --- Major mode for Prajna programming language -*- lexical-binding: t; -*-

;; Author: Meritamen <meritamen@sdf.org>
;; Version: 0.1
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/prajna-lang/prajna-mode

;;; Commentary:

;; This major mode provides syntax highlighting for the Prajna programming language.
;; Based on the provided VSCode grammar and configuration.

;;; Code:

(defgroup prajna nil
  "Major mode for editing Prajna code."
  :prefix "prajna-"
  :group 'languages)

(defcustom prajna-indent-offset 4
  "Indentation offset for Prajna code."
  :type 'integer
  :group 'prajna)

;; Keywords
(defconst prajna-keywords
  '("module" "struct" "implement" "interface" "template"
    "var" "use" "as" "if" "else" "while" "for" "in" "to"
    "continue" "break" "return" "func")
  "Keywords in Prajna language.")

;; Operators
(defconst prajna-operators
  '("=" "+" "-" "*" "/" "%" "&" "|" "!" "^")
  "Operators in Prajna language.")

;; Font lock keywords
(defconst prajna-font-lock-keywords
  `(
    ;; Keywords
    (,(regexp-opt prajna-keywords 'words) . font-lock-keyword-face)

    ;; Function/struct/template definitions
    ("\\b\\(func\\|struct\\|implement\\|template\\)\\s-+\\(\\w+\\)\\b"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))

    ;; Numbers
    ("\\b\\([0-9]+\\)\\b" . font-lock-constant-face)

    ;; Strings
    ("\"\\(?:[^\"\\\\]\\|\\\\.\\)*\"" . font-lock-string-face)

    ;; Single quoted strings
    ("'\\(?:[^'\\\\]\\|\\\\.\\)*'" . font-lock-string-face)

    ;; Operators
    (,(regexp-opt prajna-operators) . font-lock-builtin-face)

    ;; Line comments
    ("//.*$" . font-lock-comment-face)

    ;; Block comments
    ("/\\*\\(?:[^*]\\|\\*[^/]\\)*\\*/" . font-lock-comment-face)
    )
  "Font lock keywords for Prajna mode.")

;; Syntax table
(defvar prajna-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)

    ;; Brackets
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)

    ;; Operators
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?^ "." table)
    (modify-syntax-entry ?= "." table)

    table)
  "Syntax table for Prajna mode.")

;; Indentation
(defun prajna-indent-line ()
  "Indent current line as Prajna code."
  (interactive)
  (let ((indent-col 0)
        (cur-indent (current-indentation)))
    (save-excursion
      (beginning-of-line)
      (if (bobp)
          (setq indent-col 0)
        (let ((not-indented t))
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*$")
                nil
              (if (looking-at "^.*{[ \t]*$")
                  (progn
                    (setq indent-col (+ (current-indentation) prajna-indent-offset))
                    (setq not-indented nil))
                (if (looking-at "^[ \t]*}")
                    (progn
                      (setq indent-col (current-indentation))
                      (setq not-indented nil))
                  (progn
                    (setq indent-col (current-indentation))
                    (setq not-indented nil)))))))))

    (save-excursion
      (beginning-of-line)
      (if (looking-at "^[ \t]*}")
          (setq indent-col (- indent-col prajna-indent-offset))))

    (if (< indent-col 0)
        (setq indent-col 0))

    (if (/= cur-indent indent-col)
        (progn
          (beginning-of-line)
          (delete-horizontal-space)
          (indent-to indent-col)))))

;; Keymap
(defvar prajna-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "TAB") 'prajna-indent-line)
    (define-key map (kbd "C-c C-c") 'comment-region)
    (define-key map (kbd "C-c C-u") 'uncomment-region)
    map)
  "Keymap for Prajna mode.")

;; Auto-pairing
(defun prajna-electric-pair ()
  "Electric pairing for Prajna mode."
  (when (and (boundp 'electric-pair-mode) electric-pair-mode)
    (setq-local electric-pair-pairs
                '((?{ . ?})
                  (?\[ . ?\])
                  (?\( . ?\))
                  (?\" . ?\")
                  (?\' . ?\')))))

;; Comment functions
(defun prajna-comment-dwim (arg)
  "Comment or uncomment current line or region.
ARG is passed to `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-start "// ")
        (comment-end ""))
    (comment-dwim arg)))

;; Define the major mode
;;;###autoload
(define-derived-mode prajna-mode prog-mode "Prajna"
  "Major mode for editing Prajna programming language files."
  :syntax-table prajna-mode-syntax-table

  ;; Font lock
  (setq-local font-lock-defaults '(prajna-font-lock-keywords))

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*")

  ;; Indentation
  (setq-local indent-line-function 'prajna-indent-line)
  (setq-local tab-width prajna-indent-offset)

  ;; Electric pairing
  (prajna-electric-pair)

  ;; Imenu support
  (setq-local imenu-generic-expression
              '(("Functions" "^\\s-*func\\s-+\\(\\w+\\)" 1)
                ("Structs" "^\\s-*struct\\s-+\\(\\w+\\)" 1)
                ("Templates" "^\\s-*template\\s-+\\(\\w+\\)" 1)
                ("Modules" "^\\s-*module\\s-+\\(\\w+\\)" 1)))

  ;; Set up syntax highlighting
  (font-lock-mode 1))

;; Auto-mode-alist
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.prajna\\'" . prajna-mode))

(provide 'prajna-mode)

;;; prajna-mode.el ends here
