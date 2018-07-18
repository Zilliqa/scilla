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
  '("String" "Int32" "Int64" "Int128" "Uint32" "Uint64" "Uint128"
    "Int256" "Uint256" "BNum" "Address" "Hash" "Message" "Map" "ADT"))

(defvar scilla-keywords
  '("builtin" "block" "library" "let" "in" "match" "with" "end"
    "fun" "tfun" "contract" "transition" "send" "field" "accept"
    "Emp" "import"))

(defvar scilla-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( ". 1" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\) ". 4" st)
    st)
  "Syntax table for `scilla-mode'.")

(defvar scilla-font-lock-keywords
  `(
    ("\(\\*.*\\*\)" 0 font-lock-comment-face t)
    ;; stuff between double quotes
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
;; Set to use only spaces for indentation, no tabs.
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)

;; Rule 1: Beginning of buffer: column 0
;; Rule 2: Previous line contains "(let.*=|transition)"
;;             indent forward
;; Rule 3: If previous line has "end|send": indent back
;; Rule 4: If line begins with "|", find matching "match" and indent to that.
;; Rule 5: If previous line has "{" but not "}", indent forward and if
;;         previous line has "}" but not "{",
;;         find matching "{" and indent to that line
;; Rule 6: If previous line contains "*=>[ \t]*$"
;;         but current line is not fun:
;;            indent forward.
;; Rule 7: If line begins with "end", find matching "match/transition"
;; Else: Same as previous line.

(defun scilla-indent-line ()
  "Return the column to which the current line should be indented."
  (interactive)
  (setq cur-col (current-column))
  (beginning-of-line)
  (if (bobp)  ; Check for rule 1
      (indent-line-to 0)
    (let ((indented nil) cur-indent (cur-line (+ (count-lines 1 (point)) 1)) (cur-is nil))
      (save-excursion
        (progn
          ;; Match Rule 4
          (if (looking-at "[ \t]*[|]")
              (let ((ends-seen 0) (matches-seen 0) (lines-seen 0))
                (while (and (not indented) (< lines-seen 100))
                   (progn
                     (if (looking-at "[ \t]*end") (setq ends-seen (+ ends-seen 1)))
                     (if (looking-at "[ \t]*match")(setq matches-seen (+ matches-seen 1)))
                     ;; (message "Line %d: %d matches and %d ends seen" cur-line matches-seen ends-seen)
                     (if (> matches-seen ends-seen)
                         (progn
                           ;; (message "Line %d: rule 4 matched" cur-line)
                           (setq cur-indent (current-indentation))
                           (setq indented 1)
                           )
                       )
                     (forward-line -1)
                     (setq lines-seen (+ lines-seen 1))
                     )
                   )
                )
            )
          ;; Match Rule 7
          (if (looking-at "[ \t]*end")
              (let ((ends-seen 0) (matches-seen 0) (lines-seen 0))
                (while (and (not indented) (< lines-seen 1000))
                  (progn
                    (forward-line -1)
                    (setq lines-seen (+ lines-seen 1))
                    (if (looking-at "[ \t]*end") (setq ends-seen (+ ends-seen 1)))
                    (if (looking-at "[ \t]*\\(match\\|transition\\)")(setq matches-seen (+ matches-seen 1)))
                    ;; (message "Line %d: %d matches and %d ends seen" cur-line matches-seen ends-seen)
                    (if (> matches-seen ends-seen)
                        (progn
                          ;; (message "Line %d: rule 7 matched" cur-line)
                          (setq cur-indent (current-indentation))
                          (setq indented 1)
                          )
                      )
                    )
                  )
                )
            )
          (if (looking-at "[ \t]*fun")
              (setq cur-is 'fun)
            )
          (forward-line -1)
          ;; Match Rule 2
          (if (and (not indented) (looking-at "[ \t]*\\(transition\\|let.*=[ \t]*$\\)"))
              (progn
                ;; (message "Line %d: rule 2 matched" cur-line)
                (setq cur-indent (+ (current-indentation) default-tab-width))
                (setq indented 1)
                )
            )
          ;; Match Rule 6
          (if (and
               (looking-at ".*=>[ \t]*$")
               (not (eq cur-is 'fun))
               )
              (progn
                ;; (message "Line %d: rule 6 matched" cur-line)
                (setq cur-indent (+ (current-indentation) default-tab-width))
                (setq indented 1)
                )
            )
          ;; Match Rule 3
          (if (and (not indented) (looking-at "[ \t]*s?end"))
              (progn
                ;; (message "Line %d: rule 3 matched" cur-line)
                (setq cur-indent (- (current-indentation) default-tab-width))
                (setq indented 1)
                )
            )
          ;; Match Rule 5
          (if (and (not indented) (looking-at ".*{.*") (not (looking-at "^.*}.*$")))
              (progn
                ;; (message "Line %d: Rule 5a matched. \"{\" seen in previous line." cur-line)
                ;; Find location of "{".
                (re-search-forward "{")
                (setq cur-indent (current-column ))
                (setq indented 1)
                )
            (if (and (not indented) (looking-at "^.*}.*$") (not (looking-at ".*{.*")))
                ;; We have a "}". Search upwards for "{"
                (let ((num-lines 0))
                  (while (and (not indented) (< num-lines 100))
                    (progn
                      (forward-line -1)
                      (if (looking-at ".*{")
                          (progn
                            ;; (message "Line %d: Rule 5b matched. Indenting to \"{\" found." cur-line)
                            (setq cur-indent (current-indentation))
                            (setq indented 1)
                            )
                        )
                      )
                    )
                  )
                )
            )
          ;; No match, just set to previous line.
          (if (not indented)
              (progn
                ;; (message "Line %d: no match, setting to previous line" cur-line)
                (setq cur-indent (current-indentation))
                (setq indented 1)
                )
            )
          )
        )
      ;; Take action.
      (let ((d))
        (progn
          (setq d (- cur-col (current-indentation)))
          (if indented
              (indent-line-to cur-indent)
            (indent-line-to 0)
            )
          (if (> d 0)
              (forward-char d)
            )
          ;; If we're before the first non-white character, move forward
          (if (< (current-column) cur-indent)
              (move-to-column cur-indent)
            )
          )
        )
      )
    )
  )

 ;;;###autoload
(define-derived-mode scilla-mode fundamental-mode "Scilla"
  "A major mode for editing scilla files."
  :syntax-table scilla-mode-syntax-table
  (setq-local comment-start "(*")
  (setq-local comment-end "*)")
  (setq-local font-lock-defaults '(scilla-font-lock-keywords))
  (setq-local indent-line-function 'scilla-indent-line)
  )

(provide 'scilla-mode)
(add-to-list 'auto-mode-alist '("\\.scilla\\'" . scilla-mode))

 ;;; scilla.el ends here
