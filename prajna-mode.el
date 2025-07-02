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

(defvar prajna-keywords
  '("module" "struct" "implement" "interface" "template"
    "var" "use" "as" "if" "else" "while" "for" "in" "to"
    "continue" "break" "return"))

(defvar prajna-keywords-regexp
  (regexp-opt prajna-keywords 'words))

(defvar prajna-unit-keywords
  '("func" "struct" "implement" "template"))

(defvar prajna-unit-keywords-regexp
  (concat "\\b\\(" (regexp-opt prajna-unit-keywords) "\\)\\s-+\\(\\w+\\)"))

(defvar prajna-font-lock-keywords
  `(
    ;; Keywords
    (,prajna-keywords-regexp . font-lock-keyword-face)

    ;; Function/Struct/Implement/Template definitions
    (,prajna-unit-keywords-regexp
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))

    ;; Numbers
    ("\\b\\([0-9]+\\)\\b" . font-lock-constant-face)

    ;; Strings
    ("\"\\([^\"\\]\\|\\\\.\\)*\"" . font-lock-string-face)

    ;; Operators
    ("[=+\\-*/%&|!^]" . font-lock-builtin-face)

    ;; Comments - single line
    ("//.*$" . font-lock-comment-face)

    ;; Comments - block
    ("/\\*\\(.\\|\n\\)*?\\*/" . font-lock-comment-face)
    ))

(defvar prajna-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; C++ style comments "// ..."
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n ">" st)
    ;; String
    (modify-syntax-entry ?\" "\"" st)
    ;; Brackets
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?} "){" st)
    (modify-syntax-entry ?[ "(]" st)
    (modify-syntax-entry ?] ")[" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    st))

;;;###autoload
(define-derived-mode prajna-mode prog-mode "Prajna"
  "Major mode for editing Prajna language files."
  :syntax-table prajna-mode-syntax-table
  (setq font-lock-defaults '((prajna-font-lock-keywords)))
  (setq comment-start "// ")
  (setq comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.prajna\\'" . prajna-mode))

(provide 'prajna-mode)

;;; prajna-mode.el ends here
