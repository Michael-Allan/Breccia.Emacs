;;; breccia-mode.el --- A major mode for editing Breccian text  -*- lexical-binding: t; -*-

;; Copyright © 2019-2020 Michael Allan.

;; Author: Michael Allan <mike@reluk.ca>
;; Version: 0-snapshot
;; SPDX-License-Identifier: MIT
;; Package-Requires: (cl-lib)
;; Keywords: wp, outlines
;; URL: http://reluk.ca/project/Breccia/Emacs/

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package introduces a major mode for editing Breccian text (`breccia-mode`).
;; For more information, see `http://reluk.ca/project/Breccia/Emacs/`.
;;
;; If you install this package using a package manager, then already `breccia-mode` should activate
;; for any `.brec` file you load.  Alternatively you may want to install the mode manually:
;;
;;   1. Put a copy of the present file on your load path.
;;      https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Search.html
;;
;;   2. Optionally compile that copy.  E.g. load it into an Emacs buffer and type
;;      `M-x emacs-lisp-byte-compile`.
;;
;;   3. Add the following code to your initialization file.
;;      https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html
;;
;;         (autoload 'breccia-mode "breccia-mode" nil t)
;;         (set 'auto-mode-alist (cons (cons "\\.brec\\'" 'breccia-mode) auto-mode-alist))
;;
;; For a working example of manual installation, see the relevant lines
;; of `http://reluk.ca/.emacs.d/lisp/initialization.el`, and follow the reference there.

;;; Code:

;; For anyone coding a derivation of Breccia Mode, see `brec-command-matcher-components`.


(eval-when-compile (require 'cl-lib)); For macro `cl-assert`.



;; ══════════════════════════════════════════════════════════════════════════════════════════════════════
;;  P r e l i m i n a r y   d e c l a r a t i o n s
;; ══════════════════════════════════════════════════════════════════════════════════════════════════════


(defconst brec-gap-pattern
  (concat; The gap comprises one or more of the following.
   "\\(?:^ *[ \\].*$"; Indentation blind or comment block.
   "\\| \\\\.*$"; Comment appender.
   "\\| "; Space.
   "\\|\n"; Newline character.
   "\\)+") "\
The regular-expression pattern of a gap in a descriptor.")



(defconst brec-gap-start-pattern "[ \n]" "\
The regular-expression pattern of the leading character of a descriptor gap.");



(defconst brec-seg-start-pattern
  (concat "^ \\{4\\}*\\(?:"; Perfectly indented, the start of the segment comprises [SPC]
    ;; any sequence outside the delimiter of either a comment block or an indentation blind.
    "\\\\+[^ \n\\]\\|[^[:space:]\\]\\)") "\
The pattern of the start of a fontification segment other than a document head.
See also ‘brec-seg-end’."); For the definition of ‘fontification segment’, see `brec-seg-end`.



(defvar font-lock-beg); Because Font Lock omits to export these definitions. [FV]
(defvar font-lock-end)



;; ══════════════════════════════════════════════════════════════════════════════════════════════════════
;;  D e c l a r a t i o n s   i n   l e x i c o g r a p h i c   o r d e r
;; ══════════════════════════════════════════════════════════════════════════════════════════════════════


(defface brec-alarm-bullet
  `((t . (:inherit (brec-bullet font-lock-warning-face)))) "\
The face for the bullet of an alarm point."
  :group 'breccia)



(defface brec-alarm-bullet-punctuation
  `((t . (:inherit brec-alarm-bullet :weight normal))) "\
The face for any non-alphanumeric character of an alarm bullet other than
those of ‘brec-alarm-bullet-singleton’ and ‘brec-alarm-bullet-terminator’."
  :group 'breccia)



(defface brec-alarm-bullet-singleton
  `((t . (:inherit brec-alarm-bullet))) "\
The face for an alarm bullet that comprises ‘!!’ alone."
  :group 'breccia)



(defface brec-alarm-bullet-terminator
  `((t . (:inherit brec-alarm-bullet-punctuation))) "\
The face for the bullet terminator ‘!!’ of an alarm point.
Cf. ‘brec-alarm-bullet-singleton’."
  :group 'breccia)



(defface brec-aside-bullet
  `((t . (:inherit (brec-bullet brec-aside-descriptor)))) "\
The face for the bullet of an aside point."
  :group 'breccia)



(defface brec-aside-descriptor
  `((t . (:inherit shadow))) "\
The face for the descriptor of an aside point."
  :group 'breccia)



(defconst brec-backquoted-pattern-pattern "`\\(?:\\\\.\\|[^\\`]\\)+`"
  ;;                                       ╵     └────┘  └────┘    ╵
  ;;                                       Q       BC      NQ      Q
  ;;
  ;; Each element between the backquotes (Q) is either a blackslashed character pair (BC) such as “\n”
  ;; or “\`”, or a single character that is neither a backslash, nor a backquote (NQ).
  ;; See also `https://stackoverflow.com/q/249791/2402790`.
  "\
The regular-expression pattern of a regular-expression pattern complete with delimiters.")



(defun brec-backward-paragraph ()
  "A Breccian variant of the ‘backward-paragraph’ command.  Moves backward
to the previous paragraph, as defined by ‘brec-interparagraph-line-offset’."
  (interactive "^")
  (brec-move-by-lines
   (brec-interparagraph-line-offset -1)
   t "previous line"))



(defface brec-bullet
  `((t . (:inherit bold))) "\
The face for a bullet."
  :group 'breccia)



(defgroup breccia nil "\
A major mode for editing Breccian text"
  :group 'text :group 'faces
  :prefix "brec-"
  :link '(url-link "http://reluk.ca/project/Breccia/Emacs/"))



(defface brec-command-bullet
  `((t . (:inherit (brec-bullet brec-command-descriptor)))) "\
The face for the bullet of a command point."
  :group 'breccia)



(defface brec-command-descriptor
  `((t . (:inherit font-lock-builtin-face))) "\
The face for the descriptor of a command point."
  :group 'breccia)



(defface brec-command-keyword
  `((t . (:inherit brec-command-descriptor))) "\
The face for a keyword in the descriptor of a command point."
  :group 'breccia)



(defvar brec-command-matcher-components
  (let ((bq-pat brec-backquoted-pattern-pattern)
        (gap brec-gap-pattern))
    (list

     ;; Initial component
     ;; ─────────────────
     (concat
      ":" gap "\\(?:\\(?1:private\\)\\(?:" gap "\\)?$"; Parental privatizer.
      "\\|\\(?:privately" gap "\\)?\\(?:"); Optional autoprivatizer to carry the regular command (below).

     ;; Associative reference
     ;; ─────────────────────
     (concat
      "\\(?:\\(?1:re\\)" gap bq-pat gap "\\)?"; Referrer clause.

      ;; imperative clause, one of cases A and B
      ;; ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈
      ;; A. It may start with a referential command that *cannot* infer a referent.
      "\\(?:\\(?2:cf\\.\\|e\\.g\\.\\|i\\.e\\.\\|NB\\|N\\.B\\.\\|see" gap "also\\|viz\\.\\)" gap
      "\\(?:\\(?4:@\\)"; That means it must be followed *not* by an ‘@’ (error faced),
                     ;;; *nor* by any of the terms below (likewise error faced),
      "\\|\\(?4:same\\|similar\\|head\\|term\\)\\(?:" brec-gap-start-pattern "\\|$\\)"
      "\\|[^ \n \\]\\)"; but by an explicit referent indicator, the leading character of which (left)
        ;;; is sought on the pattern of a gap boundary, namely a character adjacent to a gap.
        ;;; The no-break space ‘ ’ (A0) would be redundant in this pattern if the `gap` quantifier
        ;;; were possessive (consuming indentation blinds even where it caused the match to fail).
        ;;; But Emacs does not support possessive quantifiers.

      ;; B. Alternatively it starts with a referential command that *can* infer a referent.
      "\\|\\(?2:join\\|see\\)\\>"

      ;; Order matters above: for `see also` of A to match, it must have priority over `see` of B.
      "\\)")

     ;; Parental privatizer
     ;; ───────────────────
 ;;; "\\|\\(?1:private\\)\\>"
 ;;;; But it cannot follow `privately`, and therefore comes before it (above) in the initial component.

     ;; Other command matchers, each a component
     ;; ──────────────────────
     ;; Additional matchers may be inserted here.  Open each with `\\|`.  Capture up to three,
     ;; explicitly numbered groups, e.g. `\(?1:foo\)`, `\(?2:bar\)` and `\(?3:etc\)`.
     ;; The same fontification will be given to each group.  For a working example,
     ;; see `http://reluk.ca/project/wayic/Waybrec/Emacs/waybrec-mode.el`.

     ;; Final component
     ;; ───────────────
     "\\)\\)"))
  "\
The command matcher for the command-point fontifier of ‘brec-keywords’.
Formally this is a list of string components to be concatenated in order to
form the matcher.  Derived modes may modify it before calling ‘brec-keywords’,
e.g. by inserting components that match additional commands.  Read the source
code and comments of the variable definition before attempting to do that.")



(defface brec-command-operator
  `((t . (:inherit brec-command-descriptor))) "\
The face for an operator in the descriptor of a command point."
  :group 'breccia)



(defface brec-comment-block-label
  `((t . (:inherit font-lock-doc-face))) "\
The face for a comment block label."
  :group 'breccia)



(defface brec-divider
  `((t . (:inherit font-lock-doc-face))) "\
The face for a divider."
  :group 'breccia)



(defface brec-division-inverse-labeling
  `((t . (:inherit (bold brec-divider) :inverse-video t))) "\
The face for inverse labeling and reverse video in a division label."
  :group 'breccia)



(defface brec-division-label
  `((t . (:inherit brec-divider))) "\
The face for a label in a divider."
  :group 'breccia)



(defun brec-extend-search ()
  "Ensures that the font-lock search region extends to cover the whole of its
fontification segments, bisecting none of them.  Returns nil if already it does,
non-nil otherwise."
  (save-excursion
    (let ((is-changed (brec-extend-search-up)))
      (or (brec-extend-search-down) is-changed))))



(defun brec-extend-search-down ()
  "Ensures that ‘font-lock-end’ bisects no fontification segment, moving it
forward in the buffer as necessary.  Returns nil if no change was required,
non-nil otherwise."
  (goto-char font-lock-end)
  (unless (or (bolp)(eolp)); When the prior extenders such as `font-lock-extend-region-wholelines`
    ;; do not leave `font-lock-end` at a line terminus, as usually they do, then the search
    ;; region bisects the text of the line, which means the text of a fontification segment
    ;; (a Breccian document contains nothing else), and each segment covers the whole of its lines.
    (end-of-line)); Thus far at least the present segment must extend; extend it now,
                ;;; that `re-search-forward` (below) must miss its leader.
  (let (is-changed)
    (if (re-search-forward brec-seg-start-pattern nil t); Cf. `brec-seg-end`.
        (end-of-line 0); Moving to the end of the previous line.
      (goto-char (point-max)))
    (when (< font-lock-end (point))
      (set 'font-lock-end (point))
      (setq is-changed t))
    is-changed))



(defun brec-extend-search-up ()
  "Ensures that ‘font-lock-beg’ bisects no fontification segment, moving it
backward in the buffer as necessary.  Returns nil if no change was required,
non-nil otherwise."
  (goto-char font-lock-beg)
  (end-of-line); That `re-search-backward` (below) finds any leader on the present line.
  (let (is-changed)
    (if (re-search-backward brec-seg-start-pattern nil t)
        (beginning-of-line)
      (goto-char (point-min)))
    (when (> font-lock-beg (point))
      (set 'font-lock-beg (point))
      (setq is-changed t))
    is-changed))



(defvar brec-f); [GVF]



(defface brec-forbidden-whitespace
  `((t . (:inherit font-lock-warning-face :inverse-video t))) "\
The face for disallowed, horizontal whitespace characters."
  :group 'breccia)



(defun brec-forward-paragraph ()
  "A Breccian variant of the ‘forward-paragraph’ command.  Moves forward
to the next paragraph, as defined by ‘brec-interparagraph-line-offset’."
  (interactive "^")
  (brec-move-by-lines
   (brec-interparagraph-line-offset 1)
   t "next line"))



(defvar brec-g); [GVF]



(defface brec-generic-bullet
  `((t . (:inherit (brec-bullet font-lock-keyword-face)))) "\
The face for the bullet of a generic point."
  :group 'breccia)



(defface brec-generic-bullet-punctuation
  `((t . (:inherit brec-generic-bullet :weight normal))) "\
The face for non-alphanumeric characters in the bullet of a generic point."
  :group 'breccia)



(defun brec-interparagraph-line-offset (direction)
  "Returns the offset of the next line in DIRECTION (1 or -1) that has equal
or less indentation, or nil if there is none.  If point is on a blank line,
however, then this function simply gives the offset of the next non-empty line
or terminal line of the buffer.  For this purpose a blank line is defined by
‘brec-line-is-blank’, and an empty line has no characters at all."
  (if (brec-line-is-blank)
      (brec-line-count direction 'ignore 'brec-t)
    (brec-line-count direction '> '<=)))



(defun brec-keywords ()
  "Returns the value of ‘font-lock-keywords’ to use for highlighting Breccian text."
  (list

   ;; ═══════════
   ;; Aside point
   ;; ═══════════
   ;; An aside point starts with a perfectly indented (PI) bullet comprising one slash (/).
   (list "^ \\{4\\}*\\(/\\)\\(?: +\\|$\\)"; (1) Anchoring on the bullet.
         ;; ┈──────┘
         ;;    PI

         '(1 'brec-aside-bullet)

         (list; (3, anchored highlighter) Usually a descriptor follows the bullet,
          "\\(\\(?:.\\|\n\\)+\\)";        extending thence to the end of the point head.
          '(brec-seg-end); (2, pre-form) Making the search region cover the whole of it. [PSE]
          nil '(1 'brec-aside-descriptor)))


   ;; ═════════════
   ;; Command point
   ;; ═════════════
   ;; A command point starts with a perfectly indented (PI) bullet comprising one colon (:).
   (list "^ \\{4\\}*\\(:\\)\\(?: +\\|$\\)"; (1) Anchoring on the bullet.
         ;; ┈──────┘
         ;;    PI

         '(1 'brec-command-bullet)

         ;; Descriptor
         ;; ──────────
         (list; (3, anchored highlighter) Usually a descriptor follows the bullet,
          "\\(\\(?:.\\|\n\\)+\\)";        extending thence to the end of the point head.
          '(setq; (2, pre-form)
            brec-f (point); Saving the end-bound of the anchor.
            brec-g (brec-seg-end)); Saving the limit of the present fontification segment and
              ;;; returning it, so extending the search region over the whole descriptor. [PSE]
          '(goto-char brec-f); (4, post-form) Repositioning for the next anchored highlighter, below.
          '(1 'brec-command-descriptor))

         ;; Command keywords
         ;; ────────────────
         (list; (6, anchored highlighter)
          (mapconcat 'identity brec-command-matcher-components ""); Concatenating all the
          '(progn; (5, pre-form)                                    components to one string.
             (while (progn (backward-char)                     ; Bringing the bullet ‘:’
                           (not (char-equal ?: (char-after))))); into the search region
             brec-g); and (again) ensuring the search region extends over the whole descriptor.
          '(goto-char brec-f); (7, post-form) Repositioning for the next anchored highlighter, below.
          '(1 'brec-command-keyword t t) '(2 'brec-command-keyword t t)
          '(3 'brec-command-keyword t t) '(4 'error t t))

         ;; Fractum indicator, components of
         ;; ─────────────────
         (let ((gap brec-gap-pattern))
           (list; (9, anchored highlighter)
            (concat
             gap "\\(?:\\(@\\)\\|\\(`\\)\\(\\(?:\\\\.\\|[^\\`]\\)+\\)\\(`\\)\\)")
               ;;         ╵     ╻   ╵           └────┘  └────┘          ╵
               ;;         CO    ┃   Q             BC      NQ            Q
               ;;               ╹
               ;; Each component is either a containment operator (CO) or a pattern, for which
               ;; the subcomponents (Q, BC, NQ ) are explained at `brec-backquoted-pattern-pattern`.
            '(progn; (8, pre-form)
               (while (progn (backward-char)                     ; Again bringing the bullet ‘:’
                             (not (char-equal ?: (char-after))))); into the search region
               brec-g); and ensuring the search region extends over the whole descriptor.
            nil
            '(1 'brec-command-operator t t) '(2 'brec-pattern-delimiter t t)
            '(3 'brec-pattern t t) '(4 'brec-pattern-delimiter t t))))

   ;; Regular-expression pattern, formal elements of
   ;; ──────────────────────────
   (cons; (1) Anchoring on face `brec-pattern`.
    (let (match-beg match-end)
      (lambda (limit)
        (setq match-beg (point)); Presumptively.
        (catch 'to-anchor
          (while (< match-beg limit)
            (setq match-end (next-single-property-change match-beg 'face (current-buffer) limit))
            (when (eq 'brec-pattern (get-text-property match-beg 'face))
              (set-match-data (list match-beg (goto-char match-end) (current-buffer)))
              (set 'brec-f match-beg); Saving the anchor’s bounds.
              (set 'brec-g match-end)
              (throw 'to-anchor t))
            (setq match-beg match-end))
          nil)))
    (list; (3, anchored highlighter)
     (concat
      "\\(?:\\(\\\\\\(?:[bdRt]";     \b  \d  \R  \t
         "\\|N{\\(?:[A-Z0-9 -]+";            \N{⋯}    (by name) [UCN]
            "\\|U\\+[0-9a-fA-F]+\\)}\\)\\)"; \N{U+⋯}  (by number)
      "\\|\\(\\\\\\).";                      \·  (backslash-literal pair)
      "\\|\\(\\(?:(\\(?:\\?:\\)?";   (  (?:
         "\\|[.$|)*+?^]+\\)\\)\\)"); ^^  ^  .  $  |  )  *  +  ?
     '(progn; (2, pre-form)
        (goto-char brec-f); To `match-beg` of the anchor effectively.
        brec-g); Limiting the search region (∵ return value is > point) effectively to `match-end`.
     nil '(1 'brec-pattern-element t t) '(2 'brec-pattern-element t t) '(3 'brec-pattern-element t t)))


   ;; ═══════
   ;; Divider
   ;; ═══════
   ;; A divider starts with a perfectly indented (PI) drawing or inversion sequence.
   (let* ((drawing-char "[\u2500-\u2587\u2589-\u258F\u2591-\u259F]")
          (drawing-i (concat "\\(" drawing-char "+\\(?: +" drawing-char "+\\)*\\)"))
            ;;; Capturing (i) a sequence of `drawing-char` inclusive of embedded spaces,
            ;;; yet exclusive of embedded newlines.

          (labeling-char "[^[:space:]\u2500-\u259F]")
            ;;; A division labeling character exclusive of whitespace.
          (labeling (concat labeling-char "+\\(?: +" labeling-char "+\\)*"))
            ;;; A sequence of `labeling-char` inclusive of embedded spaces,
            ;;; yet exclusive of embedded newlines.
          (labeling-i (concat "\\(" labeling "\\)"))   ; Capturing (i) an instance of labeling.
          (titling-i (concat "\n +\\(" labeling "\\)")); Capturing (i) an instance of titling.

          (inversion-mark "[\u2588\u2590]")
          (inversion-iii; Capturing (i) an inversion mark, (ii) any `labeling` together with
           ;; any space characters around it, and (iii) any full block character.
           (concat "\\(" inversion-mark "\\)\\( *\\(?:" labeling " *\\)?\\)\\(\u2588\\)?")))
     (list
      (concat "^ \\{4\\}*\\(?:" drawing-i "\\|" inversion-iii "\\)"); (1) Anchoring on the PI sequence.
      ;;       └────────┘
      ;;            PI

      '(1 'brec-divider nil t); `drawing-i`
      '(2 'brec-divider nil t)                  ; i,
      '(3 'brec-division-inverse-labeling nil t); ii and
      '(4 'brec-divider nil t)                  ; iii of `inversion-iii`.

      ;; (3, anchored highlighter) Thence it may include any mix of drawing, titling, labeling and in-
      (list (concat drawing-i "\\|" titling-i "\\|" labeling-i "\\|" inversion-iii); version sequences.
            '(brec-seg-end); (2, pre-form) Making the search region cover a whole segment of it. [PSE]
            nil; (post-form)
            '(1 'brec-divider nil t);          `drawing-i`
            '(2 'brec-titling-label nil t);    `titling-i`
            '(3 'brec-division-label nil t);  `labeling-i`
            '(4 'brec-divider nil t)                  ; i,
            '(5 'brec-division-inverse-labeling nil t); ii and
            '(6 'brec-divider nil t))))               ; iii of `inversion-iii`.


   ;; ════════════════
   ;; Free form bullet of an alarm, task or generic point
   ;; ════════════════
   (list
    (let ((rough-bullet-pattern; The best a regular expression can do here, allowing some false matches.
           (concat
            "^ \\{4\\}*\\("; Perfectly indented, the start of the bullet roughly comprises [SPC]
            "\\(?:\\\\+[\u00A0]+"; either (←) a backslash sequence preceding a no-break-space sequence,
              ;;; or (↓) zero or more backslashes preceding a character neither whitespace nor backslash.
            "\\|\\\\*\\(?:[[:alnum:]]+ *\\|[^[:alnum:][:space:]\\][\u00A0]*\\)\\)"

            ;; It ends just before either a) a space directly after a non-alphanumeric, non-space
            ;; character, or b) a newline.  Note that a no-break space (Unicode A0) will not end it.
            "\\(?:[[:alnum:]]+ *\\|[^[:alnum:][:space:]]+[\u00A0]*\\)*\\)"))
          char-first char-last is-match-changed length m1-beg m1-end m2-beg m2-end
          match-last match-end)
      (lambda (limit); Seek the next such bullet.
        (catch 'to-fontify
          (while (re-search-forward rough-bullet-pattern limit t); Starting the search on this naive
            (setq match-end (match-end 0)                        ; pattern, thence ensure each match
                  m1-beg (match-beginning 1)                     ; is correct, as follows:
                  m1-end match-end
                  m2-beg nil m2-end nil is-match-changed nil)

            (let ((end m1-end)); Trim from the match any unwanted end boundary missed above.
               ;;; It is either the start of a descriptor that starts with a comment appender
               ;;; (regular-expression pattern ‘ +\\+’) or a sequence of trailing space
               ;;; at end of the line (‘ +$’).  Trim it thus:
              (while (char-equal (char-before end) ?\\); For any trailing backslashes captured,
                (setq end (1- end)))                   ; scan backward past them.
              (while (char-equal (char-before end) ?\s); For any trailing space characters,
                (setq end (1- end)                     ; scan backward past them, and trim
                      m1-end end                       ; the whole from the captive group.
                      is-match-changed t)))
            (when
                (catch 'is-free-form-bullet
                  (setq length (- m1-end m1-beg)
                        match-last (1- m1-end); The last position in the match, that is.
                        char-last (char-after match-last))

                  ;; Task bullet
                  ;; ───────────
                  (when (char-equal ?+ char-last)
                    (if (= length 1)
                        (set 'brec-f 'brec-task-bullet-singleton)
                      (setq m2-end m1-end
                            m2-beg match-last
                            m1-end m2-beg
                            is-match-changed t)
                      (set 'brec-f 'brec-task-bullet)
                      (set 'brec-g 'brec-task-bullet-terminator))
                    (throw 'is-free-form-bullet t))

                  ;; Alarm bullet
                  ;; ────────────
                  (when (and (> length 1)
                             (char-equal ?! char-last)
                             (char-equal ?! (char-before match-last)))
                    (if (= length 2)
                        (set 'brec-f 'brec-alarm-bullet-singleton)
                      (setq m2-end m1-end
                            m2-beg (1- match-last)
                            m1-end m2-beg
                            is-match-changed t)
                      (set 'brec-f 'brec-alarm-bullet)
                      (set 'brec-g 'brec-alarm-bullet-terminator))
                    (throw 'is-free-form-bullet t))

                  ;; Miscapture of non-bullet (divider) or non-free-form (aside|command) bullet
                  ;; ──────────
                  (setq char-first (char-after m1-beg))
                  (when (and (= 1 length)                    ; When an aside or command bullet
                             (or (char-equal ?/ char-first)  ; is captured, abandon the match
                                 (char-equal ?: char-first))); and continue seeking.
                    (throw 'is-free-form-bullet nil))
                  (when (and (>= char-first ?\u2500) ; When a drawing character leads the match,
                             (<= char-first ?\u259F)); abandon the match and continue seeking.
                    (throw 'is-free-form-bullet nil))

                  ;; Generic bullet
                  ;; ──────────────
                  (set 'brec-f 'brec-generic-bullet)
                  t)

              (when is-match-changed
                (set-match-data
                 (list (match-beginning 0) match-end m1-beg m1-end m2-beg m2-end (current-buffer))))
              (throw 'to-fontify t)))
          nil)))
    '(1 brec-f) '(2 brec-g nil t))

   (cons; Reface the non-alphanumeric characters of free-form bullets.
    (let (face match-beg match-end)
      (lambda (limit)
        (setq match-beg (point)); Presumptively.
        (catch 'to-reface
          (while (< match-beg limit)
            (setq face (get-text-property match-beg 'face)
                  match-end (next-single-property-change match-beg 'face (current-buffer) limit))
            (when (or (eq face 'brec-generic-bullet)
                      (eq face 'brec-task-bullet)
                      (eq face 'brec-alarm-bullet))
              (goto-char match-beg)
              (when (re-search-forward "[^[:alnum:] \u00A0]+" match-end t)
                (set 'brec-f (intern (concat (symbol-name face) "-punctuation")))
                  ;;; To the punctuation variant of the face.
                (throw 'to-reface t)))
            (setq match-beg match-end))
          nil)))
    '(0 brec-f t))


 ;;; ──  D e f e r r e d   f o n t i f i c a t i o n  ───────────────────────────────────────────────────

   ;; ═══════════════
   ;; Comment carrier
   ;; ═══════════════
   ;; A comment carrier is delimited per line by one or more backslashes (\⋯) together isolated
   ;; in whitespace.  Usually the delimiter is followed by whitespace and commentary (WC) too.
   (list "\\(?:^\\| \\)\\(\\\\+\\)\\( +.*\\)?$"; [RWC, SPC]
         ;;              └───────┘  └──────┘
         ;;                  \⋯        WC

         '(1 'font-lock-comment-delimiter-face t) '(2 'font-lock-comment-face t t)); [OCF]

   ;; Moreover, where a carrier formed as a comment block is delimited by two or more backslashes (\\⋯),
   ;; any WC should be faced as a comment block label (L).
   (cons "^ *\\\\\\{2,\\}\\( +.+\\)$" '(1 'brec-comment-block-label t)); [OCF, RWC, SPC]
     ;;;     └──────────┘  └──────┘
     ;;;         \\⋯           L


   ;; ════════════════════
   ;; Forbidden whitespace
   ;; ════════════════════
   (cons "[\t\u2000-\u200A\u202F\u205F\u3000]" '(0 'brec-forbidden-whitespace t))))
     ;;;    9, 2000 - 200A, 202F, 205F, 3000
     ;;;
     ;;; No attempt is made here to reface any no-break space (Unicode A0) that appears
     ;;; in a forbidden context.  It will already have a distinct appearance, however,
     ;;; owing to the setting herein of `nobreak-char-display`, q.v.



(defun brec-line-count (direction skip good)
  "Returns the number of lines in DIRECTION (1 or -1) that either are empty
or satisfy relation SKIP between their indentation and the original indentation,
and end with a line whose indentation satisfies predicate GOOD."
  ;; Modified from the original code posted by ShreevatsaR. [SR]
  (let ((starting-indentation (current-indentation))
         (lines-moved direction))
    (save-excursion
      (while (and (zerop (forward-line direction))
               (or (eolp); Skipping empty lines and SKIP lines.
                 (funcall skip (current-indentation) starting-indentation)))
        (setq lines-moved (+ lines-moved direction)))
      ;; Unable now to go further, which case is it?
      (if (funcall good (current-indentation) starting-indentation)
        lines-moved
        nil))))



(defun brec-line-is-blank ()
  "Answers whether the line at point contains at most whitespace."
  ;; Modified from the original code posted by PythonNut. https://emacs.stackexchange.com/a/16826/21090
  (string-match-p "\\`\\s-*$" (thing-at-point 'line t)))



(defun brec-move-by-lines (count to-preserve-column default-label)
  "Go forward COUNT lines, or backward if COUNT is negative.  If COUNT is nil,
then instead tell the user there is no DEFAULT-LABEL (string) to move to."
  ;; Modified from the original code posted by ShreevatsaR. [SR]
  (if (not count)
      (message "No %s to move to." default-label)
    (let ((saved-column (current-column)))
      (forward-line count)
      (move-to-column
       (if to-preserve-column
           saved-column
         (current-indentation))))))



(defface brec-pattern
  `((t . (:inherit brec-command-descriptor))) "\
The face for a regular-expression pattern in the descriptor of a command point."
  :group 'breccia)



(defface brec-pattern-delimiter
  `((t . (:inherit brec-command-descriptor))) "\
The face for each of the delimiters of a regular-expression pattern."
  :group 'breccia)



(defface brec-pattern-element
  `((t . (:inherit brec-pattern))) "\
The face for a formal element of a regular-expression pattern."
  :group 'breccia)



(defun brec-seg-end ()
  "Returns the end position of the present fontification segment, provided that
point is *not* at the beginning of the segment.  If point is at the beginning,
then the result is undefined.  A fontification segment is any of the following:
a document head, point head or divider segment.

See also ‘brec-seg-start-pattern’."
  (save-excursion
    (if (re-search-forward brec-seg-start-pattern nil t); Cf. `brec-extend-search-down`.
        (end-of-line 0); Moving to the end of the previous line.
      (goto-char (point-max)))
    (point)))



(defun brec-set-for-buffer (variable value)
  "Sets VARIABLE (a symbol) to VALUE.  Signals an error if the setting
is not buffer local."
  (set variable value)
  (cl-assert (local-variable-p variable)))



(defun brec-t (&rest _args)
  "Ignores any arguments and returns t.  In other words,
the opposite of ‘ignore’."
  t)



(defface brec-task-bullet
  `((t . (:inherit (brec-bullet font-lock-function-name-face)))) "\
The face for the bullet of a task point."
  :group 'breccia)



(defface brec-task-bullet-punctuation
  `((t . (:inherit brec-task-bullet :weight normal))) "\
The face for any non-alphanumeric character of a task bullet other than
those of ‘brec-task-bullet-singleton’ and ‘brec-task-bullet-terminator’."
  :group 'breccia)



(defface brec-task-bullet-singleton
  `((t . (:inherit brec-task-bullet))) "\
The face for a task bullet that comprises ‘+’ alone."
  :group 'breccia)



(defface brec-task-bullet-terminator
  `((t . (:inherit font-lock-comment-face))) "\
The face for the bullet terminator ‘+’ of a non-singleton task point.
Cf. ‘brec-task-bullet-singleton’."
  :group 'breccia)



(defface brec-titling-label
  `((t . (:inherit (bold brec-division-label)))) "\
The face for a division label that contributes to the division title, or titles."
  :group 'breccia)



;; ══════════════════════════════════════════════════════════════════════════════════════════════════════
;;  P a c k a g e   p r o v i s i o n
;; ══════════════════════════════════════════════════════════════════════════════════════════════════════


(defvar brec--autoload-guard); Bound from here to end of file load, void at all other times.

;;;###autoload
(unless (boundp 'brec--autoload-guard); To execute only on `package-initialize`, not on file load. [GDA]
  ;; Here one wishes to *append* versus cons not to override any pattern previously added by the user.
  ;; One does not, however, expect a package to demur in installing itself.  Rather let the package
  ;; *manager* mend its own bugs, and the user meantime find recourse in the means that Emacs provides.
  ;; https://stackoverflow.com/a/35949889/2402790
  (add-to-list 'auto-mode-alist (cons "\\.brec\\'" 'breccia-mode)))



;;;###autoload
(define-derived-mode breccia-mode text-mode
  "Breccia" "\
A major mode for editing Breccian text.  For more information,
see URL ‘http://reluk.ca/project/Breccia/Emacs/’."
  :group 'breccia
  (modify-syntax-entry ?\u00A0 " " breccia-mode-syntax-table); Giving to no-break spaces (Unicode A0)
  (setq-local nobreak-char-display t); whitespace syntax, and a distinct look as defined by the Emacs
     ;;; standard face `nobreak-space`. [SF]

  ;; Define paragraph bounds, e.g. for sake of the `fill-paragraph` command
  ;; ───────────────────────
;;; (set 'use-hard-newlines t); It says, ‘Automatically becomes permanently buffer-local when set.’
;;;;; Unexpectedly that wrecks rather than helps the following.
  (setq-local paragraph-start (concat brec-seg-start-pattern ".*$"))
  (setq-local paragraph-separate "^ *\\(?:\u00A0.*\\|\\\\+\\( +.*\\)?\\)?$"); [SPC]
    ;;; Blank lines, indentation blinds and comment blocks, that is.

  ;; Remap commands to their Breccian variants
  ;; ──────────────
  (let ((m breccia-mode-map))
    (define-key m [remap backward-paragraph] 'brec-backward-paragraph)
    (define-key m [remap  forward-paragraph]  'brec-forward-paragraph))

  ;; Set up Font Lock
  ;; ────────────────
;;; (brec-set-for-buffer 'font-lock-multiline t)
;;;;; This setting does not, however, seem necessary; nor does the documentation imply that it would be.
;;;;; Should fontification ever depend on *subsequent* lines, there I think this setting would at least
;;;;; speed the response to changes.  Meantime, it seems that `brec-extend-search` alone will suffice:
  (add-hook 'font-lock-extend-region-functions 'brec-extend-search t t) ; [FLE]
  (brec-set-for-buffer 'font-lock-defaults '(brec-keywords)))



(provide 'breccia-mode)


;; NOTES
;; ─────
;;   BUG  This code is incorrect.
;;
;;   FLE  Font Lock extension.  The alternative to `font-lock-extend-region-functions`, namely the
;;        little used `font-lock-extend-after-change-region-function`, appears to be a design error.
;;        https://lists.gnu.org/archive/html/bug-gnu-emacs/2015-03/msg00818.html
;;
;;   FV · Suppressing sporadic compiler warnings ‘reference to free variable’
;;        or ‘assignment to free variable’.
;;
;;   GDA  Guarded definition of autoloads.  It would be simpler to move the autoload definitions to
;;        a separate, non-executing file, except that multi-file packages are difficult to maintain.
;;
;;   GVF  A global variable for the use of fontifiers, e.g. from within forms they quote and pass
;;        to Font Lock to be evaluated outside of their lexical scope.
;;
;;   OCF  Overrides in comment-carrier fontification.  The fontifier must override (t) any fontifier
;;        of the carrier’s containing head, and must therefore follow it in `brec-keywords`.
;;            We might have tried fontifying the carrier using the syntax system, which runs earlier.
;;        Not by mere syntax tabulation, which is unable to grasp the form of Breccian comment carriers;
;;        rather we would probably have used the macro `syntax-propertize-rules` to set syntax properties
;;        on the carrier delimiters.  But then could the `subexp-highlighters` for the containing fractum
;;        have worked around the carriers, e.g. with `override` at nil?  [SBF]
;;
;;   PSE  Pre-form search extension: extending the end boundary of the search region for multi-line
;;        anchoring.  The manual warns, ‘It is generally a bad idea to return a position greater than
;;        the end of the line’ [SBF].  But here the manual appears to be wrong.
;;        https://stackoverflow.com/a/9456757/2402790
;;
;;   RWC  Refontifying whitespace in comment carriers.  It too must be refontified to override [OCF]
;;        any improper fontification arising from inverse labeling or user customization of faces.
;;
;;   SBF  Search-based fontification.
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
;;
;;   SF · Standard faces: `https://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html`
;;        and § Standard faces at `http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/faces.el`.
;;
;;   SPC  Synchronized pattern of comment carriage.  Marking an instance of a pattern or anti-pattern,
;;        one of several that together are maintained in synchrony.
;;
;;   SR · https://emacs.stackexchange.com/a/27169/21090
;;
;;   UCN  Unicode character name. https://en.wikipedia.org/wiki/Unicode_character_property#Name


;;; breccia-mode.el ends here
