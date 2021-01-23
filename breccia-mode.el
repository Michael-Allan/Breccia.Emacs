;;; breccia-mode.el --- A major mode for editing Breccian text  -*- lexical-binding: t; -*-

;; Copyright © 2019-2021 Michael Allan.

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
;; of `http://reluk.ca/.config/emacs/lisp/initialization.el`, and follow the reference there.

;;; Code:

;; For anyone coding a derivation of Breccia Mode, see `brec-command-matcher-components`.


(eval-when-compile (require 'cl-lib)); For macro `cl-assert`.



;; ══════════════════════════════════════════════════════════════════════════════════════════════════════
;;  P r e l i m i n a r y   d e c l a r a t i o n s
;; ══════════════════════════════════════════════════════════════════════════════════════════════════════


(defconst brec-body-segment-start-pattern
  (concat
   "^\\( \\{4\\}*\\)\\("; Perfectly indented, the start of the segment comprises [SPC]
     ;;; any sequence outside the delimiter of either a comment block or an indent blind.
   "\\\\+[^ \n\\]\\|[^[:space:]\\]\\)") "\
The pattern of the start of a body segment up to its first non-space character.
It captures groups (1) the indent and (2) the first non-space character.
See also ‘brec-body-segment-start-pattern-unanchored’ and ‘brec-segment-eol’.")



(defconst brec-body-segment-start-pattern-unanchored
  (substring-no-properties brec-body-segment-start-pattern 1) "\
Pattern ‘brec-body-segment-start-pattern’ without the leading anchor ‘^’.")



(defconst brec-gap-pattern
  (concat; The gap comprises one or more of the following.
   "\\(?:^ *[ \\].*\n?"; Indent blind, comment block
   "\\| \\\\.*\n?"     ; or comment appender, each together with any bounding newline character.
   "\\| "; Space.
   "\\|\n"; Newline character.
   "\\)+") "\
The regular-expression pattern of a gap in a descriptor.
See also the simpler ‘brec-preceding-gap-character-pattern’
and ‘brec-succeeding-gap-character-pattern’; for use in detecting the presence
of a gap without having to matching the whole of it, which could be lengthy.")



(defconst brec-preceding-gap-character-pattern "[ \n]" "\
The regular-expression pattern of a descriptor gap character that could
directly precede a non-gap character.  See also ‘brec-gap-pattern’.");



(defconst brec-succeeding-gap-character-pattern "[ \n]" "\
The regular-expression pattern of a descriptor gap character that could
directly follow a non-gap character.  See also ‘brec-gap-pattern’.");



(defvar font-lock-beg); Because Font Lock omits to export these definitions. [FV]
(defvar font-lock-end)



;; ══════════════════════════════════════════════════════════════════════════════════════════════════════
;;  D e c l a r a t i o n s   i n   l e x i c o g r a p h i c   o r d e r
;; ══════════════════════════════════════════════════════════════════════════════════════════════════════


(defface brec-alarm-bullet `((t . (:inherit (brec-bullet font-lock-warning-face)))) "\
The face for the bullet of an alarm point."
  :group 'breccia)



(defface brec-alarm-bullet-punctuation `((t . (:inherit brec-alarm-bullet :weight normal))) "\
The face for any non-alphanumeric character of an alarm bullet other than
those of ‘brec-alarm-bullet-singleton’ and ‘brec-alarm-bullet-terminator’."
  :group 'breccia)



(defface brec-alarm-bullet-singleton `((t . (:inherit brec-alarm-bullet))) "\
The face for an alarm bullet that comprises ‘!!’ alone."
  :group 'breccia)



(defface brec-alarm-bullet-terminator `((t . (:inherit brec-alarm-bullet-punctuation))) "\
The face for the bullet terminator ‘!!’ of an alarm point.
Cf. ‘brec-alarm-bullet-singleton’."
  :group 'breccia)



(defface brec-aside-bullet `((t . (:inherit (brec-bullet brec-aside-descriptor)))) "\
The face for the bullet of an aside point."
  :group 'breccia)



(defface brec-aside-descriptor `((t . (:inherit shadow))) "\
The face for the descriptor of an aside point."
  :group 'breccia)



(defun brec-at-body-fractum-start ()
  "Tells whether point is at the start of a body fractum.
Returns the fractum’s first non-space position if so, nil otherwise.
See also ‘brec-body-fractum-start’."
  (let ((start (brec-at-body-segment-start)))
    (when (and start (brec-is-divider-segment-successor start))
      ;; Then point starts a divider segment that directly succeeds another.
      (setq start nil)); This divider segment does not, therefore, start the division.
    start))



(defun brec-at-body-segment-start ()
  "Tells whether point is at the start of a body segment.
Returns the segment’s first non-space position if so, nil otherwise.
See also ‘brec-body-segment-start’."
  (when (and (bolp) (looking-at brec-body-segment-start-pattern-unanchored))
    (match-beginning 2)))



(defun brec-at-fractum-start ()
  "Tells whether point is at the start of a fractum.
See also ‘brec-fractum-start’."
  (when (not (eobp)); Being neither in an empty buffer, nor at the end of the buffer where nothing starts,
    ;; Moreover being at the start of either the buffer or a body fractum.
    (or (bobp) (brec-at-body-fractum-start))))



(defconst brec-backquoted-pattern-pattern "`\\(?:\\\\.\\|[^\\`]\\)+`"
  ;;                                       ╵     └────┘  └────┘    ╵
  ;;                                       Q       BC      NQ      Q
  ;;
  ;; Each element between the backquotes (Q) is either a blackslashed character pair (BC) such as “\n”
  ;; or “\`”, or a single character that is neither a backslash, nor a backquote (NQ).
  ;; See also `https://stackoverflow.com/q/249791/2402790`.
  "\
The regular-expression pattern of a regular-expression pattern complete with delimiters.")



(defun brec-backward ()
  "Moves point to the fractal start line, previous sibling, fractum or line.
If point is below the start line of the fractum, then it moves to the start
line.  Otherwise it moves to the first applicable, if any, of the previous
sibling, linear-order predecessor or preceding line.  This command preserves
the column as far as possible."
  (interactive "^")
  (let (column previous start)
    (unless (brec-in-fractum-start)
      (setq previous (brec-fractum-start)))
    (unless previous
      (setq start (brec-body-fractum-start)
            previous (brec-previous-sibling start)))
    (unless previous
      (setq previous (brec-previous-head start)))
    (unless previous
      (setq previous (line-end-position 0)); End of previous line.
      (unless (eq (char-after previous) ?\n); [NCE]
        (setq previous nil))); No previous line exists.
    (when previous
      (setq column (current-column))
      (goto-char previous)
      (move-to-column column))))



(defun brec-body-fractum-start ()
  "The indented start position of any body fractum whose head is located at point.
Returns the fractum’s first non-space position, or nil if point is outside
of a body fractum.  See also ‘brec-at-body-fractum-start’ and
‘brec-in-body-fractum-start’.  For body segments, see ‘brec-body-segment-start’.
For fracta in general, see ‘brec-fractum-start’."
  (let ((start (brec-body-segment-start))
        dsp); Divider segment predecessor.
    (when (and start (setq dsp (brec-is-divider-segment-successor start)))
      (setq start dsp)
      (while (setq dsp (brec-divider-segment-predecessor dsp))
        (setq start dsp)))
    start))



(defun brec-body-segment-start ()
  "The indented start position of any body segment at point.
Returns the segment’s first non-space position, or nil if point is outside
of a body segment.  See also ‘brec-at-body-segment-start’ and
‘brec-in-body-segment-start’.  For body fracta, see ‘brec-body-fractum-start’.
For fracta in general, see ‘brec-fractum-start’."
  (let (start)
    (unless (setq start (brec-at-body-segment-start))
      (save-excursion
        (unless (bolp)
          (beginning-of-line)
          (setq start (brec-at-body-segment-start)))
        (while (not (or start (bobp)))
          (forward-line -1)
          (setq start (brec-at-body-segment-start)))))
    start))



(defface brec-bullet `((t . (:inherit bold))) "\
The face for a bullet."
  :group 'breccia)



(defface brec-bullet-nobreak-space `((t . (:inherit brec-nobreak-space))) "\
The face for a no-break space in a free-form bullet.
This applies to alarm, task and generic bullets."
  :group 'breccia)



(defgroup breccia nil "\
A major mode for editing Breccian text"
  :group 'text :group 'faces
  :prefix "brec-"
  :link '(url-link "http://reluk.ca/project/Breccia/Emacs/"))



(defface brec-command-bullet `((t . (:inherit (brec-bullet brec-command-descriptor)))) "\
The face for the bullet of a command point."
  :group 'breccia)



(defface brec-command-descriptor `((t . (:inherit font-lock-builtin-face))) "\
The face for the descriptor of a command point."
  :group 'breccia)



(defface brec-command-keyword `((t . (:inherit brec-command-descriptor))) "\
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
      "\\|\\(?4:same\\|similar\\|head\\|term\\)\\(?:" brec-succeeding-gap-character-pattern "\\|\\'\\)"
      "\\|[^ \n \\]\\)"; but by an explicit referent indicator, the leading character of which (left)
        ;;; is sought on the pattern of a gap boundary, namely a character adjacent to a gap.
        ;;; The no-break space ‘ ’ (A0) would be redundant in this pattern if the quantifier of `gap`
        ;;; were possessive (consuming indent blinds even where it caused the match to fail).
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



(defface brec-command-operator `((t . (:inherit brec-command-descriptor))) "\
The face for an operator in the descriptor of a command point."
  :group 'breccia)



(defface brec-commentary-nobreak-space `((t . (:inherit (font-lock-comment-face brec-nobreak-space)))) "\
The face for a no-break space in a comment carrier."
  :group 'breccia)



(defface brec-comment-block-label `((t . (:inherit font-lock-doc-face))) "\
The face for a comment block label."
  :group 'breccia)



(defface brec-divider `((t . (:inherit font-lock-doc-face))) "\
The face for a divider."
  :group 'breccia)



(defun brec-divider-segment-predecessor (position)
  "Locates any linear-order divider-segment predecessor of a fractal segment.
POSITION is any position within the segment.  If the segment has a predecessor
that is a divider segment, then the return value is its first non-space
character, otherwise it is nil.  See also ‘brec-previous-body-segment’
and ‘brec-is-divider-segment-successor’."
  (setq position (brec-previous-body-segment position))
  (when (and position (brec-is-divider-segment position))
    position))



(defface brec-division-label `((t . (:inherit brec-divider))) "\
The face for a label in a divider."
  :group 'breccia)



(defun brec-extend-search ()
  "Ensures that the font-lock search region extends to cover the whole of its
fractal segments, bisecting none of them.  Returns nil if already it does,
non-nil otherwise."
  (save-excursion
    (let ((is-changed (brec-extend-search-up)))
      (or (brec-extend-search-down) is-changed))))



(defun brec-extend-search-down ()
  "Ensures that ‘font-lock-end’ bisects no fractal segment, moving it
forward in the buffer as necessary.  Returns nil if no change was required,
non-nil otherwise."
  (goto-char font-lock-end)
  (unless (or (bolp)(eolp)); When the prior extenders such as `font-lock-extend-region-wholelines`
    ;; do not leave `font-lock-end` at a line terminus, as usually they do, then the search
    ;; region bisects the text of the line, which means the text of a fractal segment
    ;; (a Breccian document contains nothing else), and each segment covers the whole of its lines.
    (end-of-line)); Thus far at least the present segment must extend; extend it now,
                ;;; that `re-search-forward` (below) must miss its leader.
  (let (is-changed)
    (if (re-search-forward brec-body-segment-start-pattern nil t); Cf. `brec-segment-eol`.
        (end-of-line 0); Moving to the end of the previous line.
      (goto-char (point-max)))
    (when (< font-lock-end (point))
      (set 'font-lock-end (point))
      (setq is-changed t))
    is-changed))



(defun brec-extend-search-up ()
  "Ensures that ‘font-lock-beg’ bisects no fractal segment, moving it
backward in the buffer as necessary.  Returns nil if no change was required,
non-nil otherwise."
  (goto-char font-lock-beg)
  (end-of-line); That `re-search-backward` (below) finds any leader on the present line.
  (let (is-changed)
    (if (re-search-backward brec-body-segment-start-pattern nil t)
        (beginning-of-line)
      (goto-char (point-min)))
    (when (> font-lock-beg (point))
      (set 'font-lock-beg (point))
      (setq is-changed t))
    is-changed))



(defvar brec-f); [GVF]



(defface brec-forbidden-whitespace `((t . (:inherit (font-lock-warning-face nobreak-space))))
  ;; This inheritance list aims to add the attributes of warning face to those of `nobreak-space`,
  ;; so treating the latter as the common attributes for whitespace that should be made visible.
  ;; The addition of the warning attributes can fail with certain user customizations, though it tends
  ;; to fail gracefully.  E.g. if the user removes the underline from `nobreak-space` and instead sets
  ;; a background colour — as opposed to the nicer way of setting inverse video — then any misplaced
  ;; no-break space might appear without its warning colour, which by default is a foreground color.
  ;; Other forbidden whitespace, however, would at least be made visible.
  "The face for a misplaced no-break space or disallowed whitespace character."
  :group 'breccia)



(defun brec-forward ()
  "Moves point to the next sibling, fractum or line.
If point is on the start line of a fractum that is followed by a sibling,
then it moves to the sibling.  Otherwise it moves to the fractal head’s
linear-order successor, if any.  Failing that, it moves to the next line,
if any.  This command preserves the column as far as possible."
  (interactive "^")
  (let (column next)
    (setq next (brec-next-sibling (brec-in-body-fractum-start)))
    (unless next
      (setq next (brec-next-head (brec-body-fractum-start))))
    (unless next
      (setq next (line-beginning-position 2)); Beginning of next line.
      (unless (eq (char-before next) ?\n); [NCE]
        (setq next nil))); No next line exists.
    (when next
      (setq column (current-column))
      (goto-char next)
      (move-to-column column))))



(defun brec-fractum-start ()
  "The start position of any fractum whose head is located at point.
Returns the fractum’s first position, or nil if the buffer is empty.
See also ‘brec-at-fractum-start’ and ‘brec-in-fractum-start’.  For body
fracta and segments, see ‘brec-body-fractum-start’ and ‘brec-body-segment-start’."
  (let ((start (brec-body-fractum-start)))
    (unless (or start (= (point-min) (point-max)))
      ;; If point is not in the head of a body fractum, and the buffer is not empty,
      (setq start (point-min))); then point must be in the document head at its start.
    start))



(defvar brec-g); [GVF]



(defface brec-generic-bullet `((t . (:inherit (brec-bullet font-lock-keyword-face)))) "\
The face for the bullet of a generic point."
  :group 'breccia)



(defface brec-generic-bullet-punctuation `((t . (:inherit brec-generic-bullet :weight normal))) "\
The face for non-alphanumeric characters in the bullet of a generic point."
  :group 'breccia)



(defun brec-in-body-fractum-start ()
  "Tells whether point is on the start line of a body fractum.
Returns the fractum’s first non-space position, or nil if point is not
on the start line of a body fractum.  See also ‘brec-body-fractum-start’."
  (if (bolp)
      (brec-at-body-fractum-start)
    (save-excursion
      (beginning-of-line)
      (brec-at-body-fractum-start))))



(defun brec-in-body-segment-start ()
  "Tells whether point is on the start line of a body segment.
Returns the segment’s first non-space position, or nil if point is not
on the start line of a body segment.  See also ‘brec-body-segment-start’."
  (if (bolp)
      (brec-at-body-segment-start)
    (save-excursion
      (beginning-of-line)
      (brec-at-body-segment-start))))



(defun brec-indent-before (position)
  "The width of space from the beginning of the line to POSITION.
Returns the difference between those two positions, or nil if any character
other than a plain space (Unicode 20) lies between them, or nil if POSITION
is out of bounds.  See also ‘current-column’ and ‘current-indentation’."
  (let (char (width 0))
    (while
        (cond

         ;; At the beginning of the line.
         ((or (= position 1)
              (eq (setq  char (char-before position)) ?\n)); [NCE]
          nil); Break the loop and return the tallied indent.

         ;; At a space.
         ((eq char ?\s); [NCE]
          (setq width (1+ width) position (1- position))); Continue the loop.

         ;; At anything else, or out of bounds (nil `char`, that is).
         (t (setq width nil)))); Break the loop and return nil.
    width))



(defface brec-indent-blind-delimiter `((t . (:inherit brec-nobreak-space))) "\
The face for the no-break spaces that delimit an indent blind."
  :group 'breccia)



(defun brec-in-fractum-start ()
  "Tells whether point is on the start line of a fractum.
See also ‘brec-fractum-start’."
  (if (bolp)
      (brec-at-fractum-start)
    (save-excursion
      (beginning-of-line)
      (brec-at-fractum-start))))



(defun brec-is-divider-drawing (char)
  "Tells whether CHAR is a divider drawing character."
 (and (>= char ?\u2500) (<= char ?\u259F)))



(defun brec-is-divider-segment (segment-start)
  "Tells whether a body segment is a divider segment.
SEGMENT-START is the position of the segment’s first non-space character.
The return value is t if the body segment is a divider segment, nil otherwise."
  (brec-is-divider-drawing (char-after segment-start)))



(defun brec-is-divider-segment-successor (segment-start)
  "Tells whether a body segment is a divider segment that directly succeeds another.
SEGMENT-START is the position of the segment’s first non-space character.
The return value is the correponding position in the preceding divider segment,
or nil if the body segment is not a divider segment or has no divider-segment
predecessor.  See also ‘brec-is-divider-segment’ and
‘brec-divider-segment-predecessor’."
  (and (brec-is-divider-segment segment-start)           ; The segment is a divider segment
       (brec-divider-segment-predecessor segment-start))); and it directly succeeds another.



(defun brec-keywords ()
  "Returns the value of ‘font-lock-keywords’ to use for highlighting Breccian text."
  (list


   ;; ═══════════
   ;; Aside point
   ;; ═══════════

   (list; An aside point starts with a perfectly indented (PI) bullet comprising one slash (/).
    "^ \\{4\\}*\\(/\\)\\(?: +\\|$\\)"; (1) Anchoring on the bullet.
    ;; ┈──────┘
    ;;    PI

    '(1 'brec-aside-bullet)

    (list; (3, anchored highlighter) Usually a descriptor follows the bullet,
     "\\(\\(?:.\\|\n\\)+\\)";        extending thence to the end of the point head.
     '(brec-segment-eol); (2, pre-form) Making the search region cover the whole of it. [PSE]
     nil '(1 'brec-aside-descriptor)))



   ;; ═════════════
   ;; Command point
   ;; ═════════════

   (list; A command point starts with a perfectly indented (PI) bullet comprising one colon (:).
    "^ \\{4\\}*\\(:\\)\\(?: +\\|$\\)"; (1) Anchoring on the bullet.
    ;; ┈──────┘
    ;;    PI

    '(1 'brec-command-bullet)

    ;; Descriptor
    ;; ──────────
    (list; (3, anchored highlighter) Usually a descriptor follows the bullet,
     "\\(\\(?:.\\|\n\\)+\\)";        extending thence to the end of the point head.
     '(setq; (2, pre-form)
       brec-f (point); Saving the end-bound of the anchor.
       brec-g (brec-segment-eol)); Saving the limit of the present fractal segment
         ;;; and returning it, so extending the search region over the whole descriptor. [PSE]
     '(goto-char brec-f); (4, post-form) Repositioning for the next anchored highlighter, below.
     '(1 'brec-command-descriptor))

    ;; Containment operators and backquoted patterns
    ;; ─────────────────────────────────────────────
    (let ((pre-gap brec-preceding-gap-character-pattern)
          (post-gap brec-succeeding-gap-character-pattern))
      (list; (6, anchored highlighter)
       (concat
        pre-gap "\\(?:\\(@\\)" post-gap "\\|\\(`\\)\\(\\(?:\\\\.\\|[^\\`]\\)+\\)\\(`\\)\\)")
       ;;                ╵                 ╻   ╵           └────┘  └────┘          ╵
       ;;                CO                ┃   Q             BC      NQ            Q
       ;;                                  ╹
       ;; Matching either a containment operator CO or backquoted pattern, the latter comprising
       ;; subcomponents Q, BC and NQ which are explained at `brec-backquoted-pattern-pattern`.
       '(progn; (5, pre-form)
          (while (progn (backward-char)       ; Bringing the bullet ‘:’
                        (/= ?: (char-after)))); into the search region
          brec-g); and (again) ensuring it extends over the whole descriptor.
       '(goto-char brec-f); (7, post-form) Repositioning for the next anchored highlighter, below.
       '(1 'brec-command-operator t t) '(2 'brec-pattern-delimiter t t)
       '(3 'brec-pattern t t) '(4 'brec-pattern-delimiter t t)))

    ;; Command keywords (last that any `error` face it applies might override the foregoing)
    ;; ────────────────
    (list; (9, anchored highlighter)
     (mapconcat 'identity brec-command-matcher-components ""); Concatenating all the
     '(progn; (8, pre-form)                                    components to one string.
        (while (progn (backward-char)       ; Again bringing the bullet ‘:’
                      (/= ?: (char-after)))); into the search region
        brec-g); and ensuring it extends over the whole descriptor.
     nil
     '(1 'brec-command-keyword t t) '(2 'brec-command-keyword t t)
     '(3 'brec-command-keyword t t) '(4 'error t t)))


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

   (let*
       ((drawing-char "[\u2500-\u259F]")
        (drawing-cap (concat "\\(" drawing-char "+\\(?: +" drawing-char "+\\)*\\)"))
          ;;; Capturing a sequence of `drawing-char` inclusive of embedded spaces,
          ;;; yet exclusive of embedded newlines.

        (labeling-char "[^[:space:]\u2500-\u259F]")
          ;;; A division labeling character exclusive of whitespace.
        (labeling (concat labeling-char "+\\(?:[[:blank:]]+" labeling-char "+\\)*"))
          ;;; A sequence of `labeling-char` inclusive of horizontal whitespace. [PSA]
        (labeling-cap (concat "\\(" labeling "\\)"))    ; Capturing an instance of non-title labeling.
        (titling-cap (concat "\n +\\(" labeling "\\)"))); Capturing an instance of titling.

     (list; A divider starts with a perfectly indented (PI) drawing sequence.
      (concat "^ \\{4\\}*" drawing-cap); (1) Anchoring on that sequence.
      ;;       └────────┘
      ;;            PI

      '(1 'brec-divider nil t)

      ;; (3, anchored highlighter) Thence it may include any mix of drawing, titling and labeling.
      (list (concat drawing-cap "\\|" titling-cap "\\|" labeling-cap)
            '(brec-segment-eol); (2, pre-form) Extending the search region over the whole segment. [PSE]
            nil; (post-form)
            '(1 'brec-divider nil t);           `drawing-cap`
            '(2 'brec-titling-label nil t);     `titling-cap`
            '(3 'brec-division-label nil t)))); `labeling-cap`



   ;; ════════════════
   ;; Free-form bullet
   ;; ════════════════

   (list; Face each bullet of an alarm, task or generic point.
    (let ((rough-bullet-pattern; The best a regular expression can do here, allowing some false matches.
           (concat
            "^ \\{4\\}*\\("; Perfectly indented, the start of the bullet roughly comprises [SPC]
            "\\(?:\\\\+[\u00A0]"; either (←) a backslash sequence preceding a no-break-space,
              ;;; or (↓) zero or more backslashes preceding a character neither whitespace nor backslash.
            "\\|\\\\*\\(?:[[:alnum:]]+ *\\|[^[:alnum:][:space:]\\][\u00A0]?\\)\\)"

            ;; It ends just before either a) a space directly after a non-alphanumeric, non-space
            ;; character, or b) a newline.  Note that a no-break space (Unicode A0) will not end it.
            "\\(?:[[:alnum:]]+ *\\|[^[:alnum:][:space:]]+[\u00A0]?\\)*\\)"))
              ;;; The repetition nest here could fail catastrophically.  Overall a regular expression
              ;;; is inapt for seeking bullet boundaries.  It should be replaced by a function.
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
              (while (= (char-before end) ?\\); For any trailing backslashes captured,
                (setq end (1- end)))          ; scan backward past them.
              (while (= (char-before end) ?\s); For any trailing space characters,
                (setq end (1- end)            ; scan backward past them, and trim
                      m1-end end              ; the whole from the captive group.
                      is-match-changed t)))
            (when
                (catch 'is-free-form-bullet
                  (setq length (- m1-end m1-beg)
                        match-last (1- m1-end); The last position in the match, that is.
                        char-last (char-after match-last))

                  ;; Task bullet
                  ;; ───────────
                  (when (= ?+ char-last)
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
                             (= ?! char-last)
                             (= ?! (char-before match-last)))
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
                  (when (and (= 1 length)           ; When an aside or command bullet
                             (or (= ?/ char-first)  ; is captured, abandon the match
                                 (= ?: char-first))); and continue seeking.
                    (throw 'is-free-form-bullet nil))
                  (when (brec-is-divider-drawing char-first); When a drawing character leads the match,
                    (throw 'is-free-form-bullet nil))       ; abandon the match and continue seeking.

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



   ;; ═══════════════
   ;; Comment carrier
   ;; ═══════════════

   (list; A comment carrier is delimited per line by one or more backslashes (\⋯) together isolated
      ;;; in whitespace.  Usually the delimiter is followed by whitespace and commentary (WC) too.
    "\\(?:^\\| \\)\\(\\\\+\\)\\( +.*\\)?$"; [SPC]
      ;;;           └───────┘  └──────┘
      ;;;               \⋯        WC

    '(1 'font-lock-comment-delimiter-face t) '(2 'font-lock-comment-face t t)); [OCF]


   (cons; Moreover, where a carrier formed as a comment block is delimited by two or more
      ;;; backslashes (\\⋯), any WC should be faced as a comment block label (L).
    "^ *\\\\\\{2,\\}\\( +.+\\)$" '(1 'brec-comment-block-label t)); [OCF, SPC]
    ;;; └──────────┘  └──────┘
    ;;;     \\⋯           L



   ;; ══════════
   ;; Whitespace
   ;; ══════════
   (cons
    (lambda (limit)
      (let ((p (point))
            c face found)
        (while (and (not found) (< p limit))
          (setq c (char-after p)
                face (get-text-property p 'face))
          (cond

           ;; No-break space
           ;; ──────────────
           ((= c ?\u00A0)
            (cond

             ;; In commentary.
             ((and face (memq face '(font-lock-comment-face brec-comment-block-label)))
              (setq found t brec-f 'brec-commentary-nobreak-space))

             ;; In a free-form bullet.
             ((and face (memq face '(brec-alarm-bullet brec-generic-bullet brec-task-bullet)))
              (setq found t brec-f 'brec-bullet-nobreak-space))

             ;; Delimiting an indent blind.
             ((brec-indent-before p)
              (setq found t brec-f 'brec-indent-blind-delimiter))

             ;; Misplaced no-break space.
             (t (setq found t brec-f 'brec-forbidden-whitespace))))

           ;; Forbidden character
           ;; ───────────────────
           ((or (memq c '(?\t ?\u202F ?\u205F ?\u3000))
                (and (>= c ?\u2000) (<= c ?\u200A)))
            (setq found t brec-f 'brec-forbidden-whitespace))

           (t (setq p (1+ p)))))
        (when found
          (set-match-data (list p (goto-char (1+ p)) (current-buffer)))
          t))); Then return t to Font Lock, else nil.
    '(0 brec-f prepend)))); Prepended only in case the original face is ever wanted.



(defun brec-next-head (body-segment-start)
  "Locates the linear-order successor of a fractal head.
BODY-SEGMENT-START is the position of the first non-space character of
any body segment in the head, or nil for a document head.  The return value
is the correponding position in the next head, or nil if no next head exists."
  (when body-segment-start
    (let ((next (brec-next-segment body-segment-start)))
      (when (and next (brec-is-divider-segment body-segment-start))
        (while (and next (brec-is-divider-segment next))
          (setq next (brec-next-segment next))))
      next)))



(defun brec-next-segment (position)
  "Locates the linear-order successor of a fractal segment.
POSITION is any position within the segment.  The return value is the first
non-space character in the next segment, or nil if no next segment exists."
  (let (next)
    (save-excursion
      (goto-char position)
      (while (and (= 0 (forward-line))
                  (not (setq next (brec-at-body-segment-start))))))
    next))



(defun brec-next-sibling (body-segment-start)
  "Locates the next sibling of a fractum.
BODY-SEGMENT-START is the position of the first non-space character of
any body segment in the head, or nil for a document head.  The return value is
the correponding position in the next sibling, or nil if no next sibling exists."
  (when body-segment-start
    (let ((next-head body-segment-start)
          (sib-i (brec-indent-before body-segment-start))
          i next-sibling)
      (while (and (setq next-head (brec-next-head next-head))
                  (not (or (< (setq i (brec-indent-before next-head)) sib-i); Fell out of parent.
                           (when (= i sib-i); Found the sibling.
                             (setq next-sibling next-head))))))
      next-sibling)))



(defface brec-nobreak-space `((t . (:inherit nobreak-space))) "\
The face for a no-break space (Unicode A0) in Breccia."
  :group 'breccia)



(defface brec-pattern `((t . (:inherit brec-command-descriptor))) "\
The face for a regular-expression pattern in the descriptor of a command point."
  :group 'breccia)



(defface brec-pattern-delimiter `((t . (:inherit brec-command-descriptor))) "\
The face for each of the delimiters of a regular-expression pattern."
  :group 'breccia)



(defface brec-pattern-element `((t . (:inherit brec-pattern))) "\
The face for a formal element of a regular-expression pattern."
  :group 'breccia)



(defun brec-previous-body-segment (position)
  "Locates the linear-order body-segment predecessor of a fractal segment.
POSITION is any position within the fractal segment.  The return value
is the position of the first non-space character in the preceding
body segment, or nil if no preceding body segment exists.
See also ‘brec-divider-segment-predecessor’."
  (let (previous)
    (save-excursion
      (goto-char position)
      (when (setq position (brec-body-segment-start))
        (goto-char position)
        (beginning-of-line)
        (unless (bobp)
          (forward-line -1)
          (setq previous (brec-body-segment-start)))))
    previous))



(defun brec-previous-head (start-segment-position)
  "Locates the linear-order predecessor of a fractal head.
START-SEGMENT-POSITION is any position in the first body segment of the head,
or nil for a document head.  The return value is the position of the first
non-space character in the previous head, or nil if no previous head exists."
  (when start-segment-position
    (let ((previous (brec-previous-body-segment start-segment-position))
          p)
      (when previous
        (while (setq p (brec-is-divider-segment-successor previous))
          (setq previous p)))
      previous)))



(defun brec-previous-sibling (body-fractum-start)
  "Locates the previous sibling of a fractum.
BODY-FRACTUM-START is the position of the fractum’s first non-space character,
or nil for the document fractum.  The return value is the correponding position
in the previous sibling, or nil if no previous sibling exists."
  (when body-fractum-start
    (let ((previous-head body-fractum-start)
          (sib-i (brec-indent-before body-fractum-start))
          i previous-sibling)
      (while (and (setq previous-head (brec-previous-head previous-head))
                  (not (or (< (setq i (brec-indent-before previous-head)) sib-i); Fell out of parent.
                           (when (= i sib-i); Found the sibling.
                             (setq previous-sibling previous-head))))))
      previous-sibling)))



(defun brec-segment-eol ()
  "The position at the end of the last line of the present fractal segment,
provided point is not at the beginning of the segment; otherwise the result
is undefined.

See also ‘brec-body-segment-start-pattern’."
  (save-excursion
    (if (re-search-forward brec-body-segment-start-pattern nil t); Cf. `brec-extend-search-down`.
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



(defface brec-task-bullet `((t . (:inherit (brec-bullet font-lock-function-name-face)))) "\
The face for the bullet of a task point."
  :group 'breccia)



(defface brec-task-bullet-punctuation `((t . (:inherit brec-task-bullet :weight normal))) "\
The face for any non-alphanumeric character of a task bullet other than
those of ‘brec-task-bullet-singleton’ and ‘brec-task-bullet-terminator’."
  :group 'breccia)



(defface brec-task-bullet-singleton `((t . (:inherit brec-task-bullet))) "\
The face for a task bullet that comprises ‘+’ alone."
  :group 'breccia)



(defface brec-task-bullet-terminator `((t . (:inherit font-lock-comment-face))) "\
The face for the bullet terminator ‘+’ of a non-singleton task point.
Cf. ‘brec-task-bullet-singleton’."
  :group 'breccia)



(defface brec-titling-label `((t . (:inherit (bold brec-division-label)))) "\
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
  (set 'auto-mode-alist (cons (cons "\\.brec\\'" 'breccia-mode) auto-mode-alist)))



;;;###autoload
(define-derived-mode breccia-mode text-mode
  "Breccia" "\
A major mode for editing Breccian text.  For more information,
see URL ‘http://reluk.ca/project/Breccia/Emacs/’."
  :group 'breccia

  ;; Set up no-break-space handling (Unicode A0)
  ;; ──────────────────────────────
  (modify-syntax-entry ?\u00A0 " " breccia-mode-syntax-table); Assigning whitespace syntax.
  (setq-local nobreak-char-display nil); Default application of standard face `nobreak-space`. [SF]
     ;;; Defeat it, because it applies the face by a method unamenable to override in `brec-keywords`.
     ;;; Instead let Breccia Mode face these characters using standard, Font Lock methods.

  ;; Set up paragraph handling
  ;; ─────────────────────────
  (setq-local paragraph-start brec-body-segment-start-pattern); [PBD]
  (setq-local paragraph-separate "^ *\\(?:\u00A0.*\\|\\\\+\\( +.*\\)?\\)?$"); [PBD, SPC]
    ;;; Indent blinds, comment blocks and blank lines, that is.
  (let ((m breccia-mode-map))
    (define-key m [remap backward-paragraph] #'brec-backward)
    (define-key m [remap forward-paragraph] #'brec-forward))

  ;; Hook into Font Lock
  ;; ───────────────────
;;; (brec-set-for-buffer 'font-lock-multiline t)
;;;;; This setting does not, however, seem necessary; nor does the documentation imply that it would be.
;;;;; Should fontification ever depend on *subsequent* lines, there I think this setting would at least
;;;;; speed the response to changes.  Meantime, it seems that `brec-extend-search` alone will suffice:
  (add-hook 'font-lock-extend-region-functions #'brec-extend-search t t) ; [FLE]
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
;;   NCE  Not `char-equal` or `=`, which fail if the position is out of bounds.
;;        Rather `eq` which instead gives nil in that case.
;;
;;   OCF  Overrides in comment-carrier fontification.  The fontifier must override (t) any fontifier
;;        of the carrier’s containing head, and must therefore follow it in `brec-keywords`.
;;            We might have tried fontifying the carrier using the syntax system, which runs earlier.
;;        Not by mere syntax tabulation, which is unable to grasp the form of Breccian comment carriers;
;;        rather we would probably have used the macro `syntax-propertize-rules` to set syntax properties
;;        on the carrier delimiters.  But then could the `subexp-highlighters` for the containing fractum
;;        have worked around the carriers, e.g. with `override` at nil?  [SBF]
;;
;;   PBD  Paragraph boundary definition.  It is used by the command `fill-paragraph`, for instance,
;;        not however by the commands `brec-backward-paragraph` and `brec-forward-paragraph`.
;;            The manual says it “should not use ‘^’ to anchor the match”; yet without that anchor,
;;        `fill-paragraph` fails to work, instead collapsing the fractal head to a single line.
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Regexps.html
;;
;;   PSA  Plain spaces alone are valid separators here, yet the pattern should not be broken
;;        merely on account of forbidden whitespace.  Forbidden whitespace is the separate
;;        concern of a dedicated fontifier.  See code § Whitespace.
;;
;;   PSE  Pre-form search extension: extending the end boundary of the search region for multi-line
;;        anchoring.  The manual warns, ‘It is generally a bad idea to return a position greater than
;;        the end of the line’ [SBF].  But here the manual appears to be wrong.
;;        https://stackoverflow.com/a/9456757/2402790
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
;;   UCN  Unicode character name. https://en.wikipedia.org/wiki/Unicode_character_property#Name


;;; breccia-mode.el ends here
