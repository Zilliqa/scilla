;; This is an Emacs major mode for the Scilla language.
;; Include the below line (with path set properly) in ~/.emacs
;;   (load-file "/path/to/scilla.el")

(defvar scilla-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for `scilla-mode'.")

(defvar scilla-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    st)
  "Syntax table for `scilla-mode'.")

(defvar scilla-constants
  '("False" "True" "Some" "None"))

(defvar scilla-types
  '("String" "Int" "BNum" "Address" "Hash" "Message" "Map" "ADT"))

(defvar scilla-keywords
  '("builtin" "block" "library" "let" "in" "match" "with" "end"
    "fun" "tfun" "contract" "transition" "send" "field" "accept"
    "Emp"))

(defvar scilla-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( ". 1" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\) ". 4" st)
    st)
  "Syntax table for `scilla-mode'.")

(defvar scilla-font-lock-keywords
  `(
    ;; stuff between double quotes
    ("\(\\*.*\\*\)" . font-lock-comment-face)
    ("\"\\.\\*\\?" . font-lock-string-face)
    ;; ; : , ; { } =>  @ $ = are all special elements
    ;; (":\\|,\\|;\\|{\\|}\\|=>\\|@\\|$\\|=" . font-lock-keyword-face)
    ( ,(regexp-opt scilla-keywords 'words) . font-lock-keyword-face)
    (, (regexp-opt scilla-types 'words) . font-lock-type-face)
    ;; Some known constants like True/False etc.
    ( ,(regexp-opt scilla-constants 'words) . font-lock-constant-face)
    ;; Numerical constants. Decimal or Hexadecimal integers.
    ("\\(\\b[0-9]+\\b\\|\\b0x[0-9a-fA-F]+\\b\\)" . font-lock-constant-face)
    ;; Math any other identifier
    ("[a-zA-Z_]+[a-zA-Z0-9]*" . font-lock-variable-face)
    ))

;;; Indentation
;; TODO

(defun scilla-calculate-indentation ()
  "Return the column to which the current line should be indented.")

 ;;;###autoload
(define-derived-mode scilla-mode fundamental-mode "Scilla"
  "A major mode for editing scilla files."
  :syntax-table scilla-mode-syntax-table
  (setq-local comment-start "(*")
  (setq-local comment-end "*)")
  (setq-local font-lock-defaults	'(scilla-font-lock-keywords))
  ;;(setq-local indent-line-function 'scilla-indent-line)
  )

(provide 'scilla-mode)
(add-to-list 'auto-mode-alist '("\\.scilla\\'" . scilla-mode))

 ;;; scilla.el ends here
