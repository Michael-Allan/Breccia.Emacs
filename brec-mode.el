;;; brec-mode.el --- A major mode for editing Breccian text  -*- lexical-binding: t; -*-

;; Copyright © 2019-2024 Michael Allan.
;;
;; Author: Michael Allan <mike@reluk.ca>
;; Version: 0-snapshot
;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "24.3"))
;; Keywords: outlines, wp
;; URL: http://reluk.ca/project/Breccia/Emacs/
;;
;; This file is not part of GNU Emacs.
;;
;; This file is released under an MIT licence.  A copy of the licence normally accompanies it.
;; If not, then see `http://reluk.ca/project/Breccia/Emacs/LICENCE.txt`.

;;; Commentary:

;;   This package implements Brec Mode, a major mode for editing Breccia.
;;   Breccia is a lightweight markup language for point-form outlining and drafting.
;;   For more information, see `http://reluk.ca/project/Breccia/`
;;   and `http://reluk.ca/project/Breccia/Emacs/`.
;;
;; Installation
;;
;;   If you installed this package from MELPA using a package manager, then already Brec Mode should
;;   activate for any `.brec` file you load.  Alternatively, you may want to install it manually:
;;
;;       1. Put a copy of the file `brec-mode.el` on your load path.
;;          https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Search.html
;;
;;       2. Optionally compile that copy.  Load it into an Emacs buffer, for example,
;;          and type `M-x emacs-lisp-byte-compile`.
;;
;;       3. Add the following code to your Emacs initialization file.
;;
;;             (autoload 'brec-mode "brec-mode" nil t)
;;             (set 'auto-mode-alist (cons (cons "\\.brec\\'" 'brec-mode) auto-mode-alist))
;;             (register-definition-prefixes "brec-mode" '("brec-"))
;;
;;   For a working example, see the relevant lines of `http://reluk.ca/.config/emacs/initialization.el`.
;;
;; Customization
;;
;;   To see a list of customizeable faces and variables, enter a Brec Mode buffer, or otherwise load
;;   Brec Mode, and type `M-x customize-group <RET> brec <RET>`.  Alternatively, look through
;;   the `defcustom` and `defface` definitions of file `brec-mode.el`.
;;
;;   For a working example, see:
;;
;;       • The author’s initialization file — http://reluk.ca/.config/emacs/initialization.el
;;       • The author’s `~/.Xresources` — http://reluk.ca/.Xresources

;;; Code:

;; For anyone coding a derivation of Brec Mode, see `brec-command-matcher-components`.


(eval-when-compile (require 'cl-lib)); Built into Emacs since version 24.3.



;; ══════════════════════════════════════════════════════════════════════════════════════════════════════
;;  P r e l i m i n a r y   d e c l a r a t i o n s
;; ══════════════════════════════════════════════════════════════════════════════════════════════════════


(defconst brec-gap-pattern
  (concat; The gap comprises one or more of the following.
   "\\(?:^ *[ \\].*$"; Indent blind, comment block
   "\\| \\\\.*$"     ; or comment appender, each together with any bounding newline. [LLT]
   "\\| "; Space.
   "\\|\n"; Newline.
   "\\)+")
  "The regular-expression pattern of a gap in a descriptor.
See also the simpler ‘brec-preceding-gap-character-pattern’
and ‘brec-succeeding-gap-character-pattern’; for use in detecting the presence
of a gap without having to matching the whole of it, which could be lengthy.")



(defconst brec-math-block-delimiter-char ?･; Halfwidth katakana middle dot (FF65).
  "The delimiter character for block-form mathematics.")



(defconst brec-math-inline-delimiter-char ?\u2060; Word joiner.
  "The delimiter character for in-line mathematics.")



(defconst brec-pattern-matcher-pattern "`\\(?:\\\\.\\|[^\n\\`]\\)+`[isp]*";  [PMP]
  ;;                                    ╵     └────┘  └──────┘    ╵└────┘
  ;;                                    Q       BC       NQ       Q   M
  ;;
  ;; Each element between the backquotes (Q) is either a blackslashed character pair (BC) such as “\n”
  ;; or “\`”, or a single character that is neither a newline, backslash, nor backquote (NQ).
  ;; See also `https://stackoverflow.com/q/249791/2402790`.
  ;;
  ;; One or more match modifiers (M) may follow the trailing quote.

  "The regular-expression pattern of a regular-expression pattern matcher.")



(defconst brec-term-end-boundary-pattern "\\(?: \\|$\\)"
  "The regular-expression pattern of the end boundary of a term.
This is either a space or a line end.");



(defvar font-lock-beg); [FV, because Font Lock omits to export these definitions]
(defvar font-lock-end)



;; ══════════════════════════════════════════════════════════════════════════════════════════════════════
;;  D e c l a r a t i o n s   i n   l e x i c o g r a p h i c   o r d e r
;; ══════════════════════════════════════════════════════════════════════════════════════════════════════


(defgroup brec nil
  "A major mode for editing Breccia."
  :group 'text :group 'faces
  :prefix "brec-"
  :link '(url-link "http://reluk.ca/project/Breccia/Emacs/"))



(defface brec-alarm-bullet
  `((t . (:inherit (brec-bullet font-lock-warning-face))))
  "The face for the bullet of an alarm point."
  :group 'brec-point-faces)



(defface brec-alarm-bullet-punctuation  `((t . (:inherit brec-alarm-bullet)))
  "The face for a non-alphanumeric character of an alarm bullet.
Cf. ‘brec-alarm-bullet-singleton’ and ‘brec-alarm-bullet-terminator’."
  :group 'brec-point-faces)



(defface brec-alarm-bullet-singleton `((t . (:inherit brec-alarm-bullet)))
  "The face for an alarm bullet that comprises \\=`!!\\=` alone."
  :group 'brec-point-faces)



(defface brec-alarm-bullet-terminator `((t . (:inherit font-lock-comment-face)))
  "The face for the bullet terminator \\=`!!\\=` of an alarm point.
Cf. ‘brec-alarm-bullet-singleton’."
  :group 'brec-point-faces)



(defface brec-aside-bullet
  `((t . (:inherit (brec-bullet brec-aside-descriptor))))
  "The face for the bullet of an aside point."
  :group 'brec-point-faces)



(defface brec-aside-descriptor `((t . (:inherit shadow)))
  "The face for the descriptor of an aside point."
  :group 'brec-point-faces)



(defun brec-at-body-fractum-start ()
  "Whether point is at the start of a body fractum.
Returns the fractum’s first non-space position if so, nil otherwise.
See also ‘brec-body-fractum-start’."
  (let ((start (brec-at-body-segment-start)))
    (when (and start (brec-is-divider-segment-successor start))
      ;; Then point starts a divider segment that directly succeeds another.
      (setq start nil)); This divider segment does not, therefore, start the division.
    start))



(defun brec-at-body-segment-start ()
  "Whether point is at the start of a body segment.
Returns the segment’s first non-space position if so, nil otherwise.
See also ‘brec-body-segment-start’."
  (defvar brec-body-segment-start-pattern-unanchored); [FV]
  (when (and (bolp) (looking-at brec-body-segment-start-pattern-unanchored))
    (match-beginning 2)))



(defun brec-at-fractum-start ()
  "Whether point is at the start of a fractum.
See also ‘brec-fractum-start’."
  (when (not (eobp)); Being neither in an empty buffer, nor at the end of the buffer where nothing starts,
    ;; Moreover being at the start of either the buffer or a body fractum.
    (or (bobp) (brec-at-body-fractum-start))))



(defun brec-backward ()
  "Move point to the fractal start line, previous sibling, fractum or line.
If point is below the start line of the fractum, then move to the start line.
Otherwise move to the first applicable, if any, of the previous sibling,
linear-order predecessor or preceding line.  This command preserves
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



(defconst brec-body-segment-start-pattern
  (concat
   "^\\( \\{4\\}*\\)\\(" ; The start of a body segment comprises indent-perfect spacing plus either:
     "\\\\+[^ \n\\]\\|"  ;  i) one or more backslashes not making a comment-block delimiter [CCP], or
     "[^[:space:]\\]\\)"); ii) anything except an indent-imperfecting space, indent-blind delimiter,
       ;;; disallowed whitespace, or backslash which — given the prior failure of (i) to match —
       ;;; comprises or leads a backslash sequence that *does* make a comment-block delimiter.
  "The pattern of the start of a body segment.
It captures groups (1) the perfect indent and (2) one or more
of the non-plain-space characters (Unicode non-20) that directly follow.
See also ‘brec-segment-end’ and ‘brec-body-segment-start-pattern-unanchored’.")



(defconst brec-body-segment-start-pattern-unanchored
  (substring-no-properties brec-body-segment-start-pattern 1)
  "Pattern ‘brec-body-segment-start-pattern’ without the leading anchor \\=`^\\=`.")



(defface brec-bullet `((t . (:inherit bold)))
  "The face for a bullet."
  :group 'brec-point-faces)



(defface brec-bullet-nobreak-space `((t . (:inherit brec-nobreak-space)))
  "The face for a no-break space in a free-form bullet.
This applies to alarm, task and plain bullets."
  :group 'brec-point-faces)



(defface brec-command-appendage `((t . (:inherit brec-aside-descriptor)))
  "The face for the content of a command appendage."
  :group 'brec-point-faces)



(defface brec-command-bullet
  `((t . (:inherit (brec-bullet brec-command-descriptor))))
  "The face for the bullet of a command point."
  :group 'brec-point-faces)



(defface brec-command-descriptor `((t . (:inherit font-lock-builtin-face)))
  "The face for the descriptor of a command point."
  :group 'brec-point-faces)



(defvar brec-command-matcher-components
  (let ((end brec-term-end-boundary-pattern); To reject any command directly followed by further
          ;;; term characters, e.g. the misplaced delimiter \\=`:\\=` of an appendage clause.
        (gap brec-gap-pattern))
    (list
     "^ \\{4\\}*: +\\(?:privately +\\)?\\(?:"; Anchoring on the perfectly indented (PI) bullet `:`,
     ;; ┈──────┘          so precluding a match that begins instead with an appendage delimiter `:`.
     ;;    PI

     ;; Afterlinker
     ;; ───────────
     (concat
      "\\(?:\\(?1:re\\)" gap brec-pattern-matcher-pattern gap "\\)?"; Optional subject clause.

      "\\(?2:see\\(?: +\\(?:also\\|e\\.g\\.\\)?\\)?\\|join\\|cf\\.\\(?: +e\\.g\\.\\)?\\|"; Referential
      "\\(?:e\\.g\\|i\\.e\\|N\\.B\\|q\\.v\\|sc\\|viz\\)\\.\\|contra\\|pace\\|NB\\)" end) ; command.

     ;; Note carrier
     ;; ────────────
     (concat
      "\\|\\(?1:ad\\|on\\)" gap brec-pattern-matcher-pattern "\\(?:" gap "\\(?2:note\\)\\)?" end
        ;;; Either a purview clause ↑ optionally followed by a note label ↑,
      "\\|\\(?1:note\\)" end); ← or a note label all alone.

     ;; Privatizer
     ;; ──────────
     "\\|\\(?1:private\\)" end

     ;; Other components, each matching one or more additional commands
     ;; ────────────────
     ;; Derived modes may insert their own components here.  Open each with `\\|`.
     ;; Capture up to four explicitly numbered groups, e.g. `\(?1:foo\)`, `\(?2:bar\)` and `\(?4:bad\)`.
     ;; Command facing will be given to any group numbered 1 to 3, error facing to any numbered 4.
     ;; For a working example, see `http://reluk.ca/project/wayic/Waybrec/Emacs/waybrec-mode.el`.

     ;; Final component, terminating the matcher
     ;; ───────────────
     "\\)"))
  "The command matcher for the command-point fontifier of ‘brec-keywords’.
Formally this is a list of string components to be concatenated in order to
form the matcher.  Derived modes may modify it before calling ‘brec-keywords’,
e.g. by inserting components that match additional commands.  Read the source
code and comments of this variable’s definition before attempting to do that.")



(defface brec-command-operator `((t . (:inherit brec-command-descriptor)))
  "The face for an operator or other key element of a command-point descriptor."
  :group 'brec-point-faces)



(defface brec-comment-appender `((t . (:inherit font-lock-comment-face)))
  "The face for the content of a comment appender."
  :group 'brec-comment-faces)



(defface brec-comment-appender-delimiter
  `((t . (:inherit font-lock-comment-delimiter-face)))
  "The face for the delimiter of a comment appender."
  :group 'brec-comment-faces)



(defface brec-commentary-nobreak-space
  `((t . (:inherit (brec-comment-block brec-nobreak-space))))
  "The face for a no-break space in a block comment."
  :group 'brec-comment-faces)



(defface brec-comment-block `((t . (:inherit font-lock-comment-face)))
  "The face for comment-block content other than a comment-block label.
See also ‘brec-comment-block-label’."
  :group 'brec-comment-faces)



(defface brec-comment-block-delimiter
  `((t . (:inherit font-lock-comment-delimiter-face)))
  "The face for the delimiter of a comment block."
  :group 'brec-comment-faces)



(defface brec-comment-block-label `((t . (:inherit font-lock-doc-face)))
  "The face for a comment-block label."
  :group 'brec-comment-faces)



(defgroup brec-comment-faces nil
  "Faces for comment carriers."
  :group 'brec-faces
  :prefix "brec-")



(defface brec-divider `((t . (:inherit font-lock-doc-face)))
  "The face for a divider."
  :group 'brec-faces)



(defun brec-divider-segment-predecessor (position)
  "Locates any linear-order divider-segment predecessor of a fractal segment.
POSITION is any position within the segment.  If the segment has a predecessor
that is a divider segment, then the return value is its first non-space
character, otherwise it is nil.  See also ‘brec-previous-body-segment’
and ‘brec-is-divider-segment-successor’."
  (setq position (brec-previous-body-segment position))
  (when (and position (brec-is-divider-segment position))
    position))



(defface brec-division-label `((t . (:inherit brec-divider)))
  "The face for a label in a divider."
  :group 'brec-faces)



(defun brec-extend-search ()
  "Ensure the font-lock search region comprises one or more whole fractal segments.
Return nil if already it does, non-nil otherwise."
  (save-excursion
    (let ((is-changed (brec-extend-search-up)))
      (or (brec-extend-search-down) is-changed))))



(defun brec-extend-search-down ()
  "Ensure that ‘font-lock-end’ bisects no fractal segment.
Move it forward in the buffer if necessary.  Return nil if no change
was required, non-nil otherwise."
  (goto-char font-lock-end)
;;(unless (or (bolp)(eolp)); When the prior extenders such as `font-lock-extend-region-wholelines`
;;  ;; do not leave `font-lock-end` at a line terminus, as usually they do, then the search
;;  ;; region bisects the text of the line, which means the text of a fractal segment
;;  ;; (a Breccian file contains nothing else), and each segment covers the whole of its lines.
;;  (end-of-line)); Thus far at least the present segment must extend, so move there now and bring
;;              ;;; point nearer to any next match of `brec-body-segment-start-pattern` (below).
;;;; not an optimization
  (let (is-changed)
    (if (re-search-forward brec-body-segment-start-pattern nil t); Cf. `brec--segment-end`.
        (end-of-line 0); Moving to the end of the previous line.
      (goto-char (point-max)))
    (when (< font-lock-end (point))
      (setq font-lock-end (point)
            is-changed t))
    is-changed))



(defun brec-extend-search-up ()
  "Ensure that ‘font-lock-beg’ bisects no fractal segment.
Move it backward in the buffer if necessary.  Return nil if no change
was required, non-nil otherwise."
  (goto-char font-lock-beg)
  (end-of-line); That `re-search-backward` (below) finds any leader on the present line.
  (let (is-changed)
    (if (re-search-backward brec-body-segment-start-pattern nil t)
        (beginning-of-line)
      (goto-char (point-min)))
    (when (> font-lock-beg (point))
      (setq font-lock-beg (point)
            is-changed t))
    is-changed))



(defvar brec-f); [GVF]



(defgroup brec-faces nil
  "Faces for Breccia."
  :group 'brec
  :prefix "brec-")



(defface brec-forbidden-whitespace `((t . (:inherit brec-transparent-error)))
  "The face for a misplaced no-break space or disallowed whitespace character."
  :group 'brec-faces)



(defun brec-forward ()
  "Move point to the next sibling, fractum or line.
If point is on the start line of a fractum that is followed by a sibling,
then move it to the sibling.  Otherwise move it to the fractal head’s
linear-order successor, if any.  Failing that, move it to the next line,
if any.  This command preserves the column as far as possible."
  (interactive "^")
  (let (column next)
    (setq next (brec-next-sibling-or-elder (brec-in-body-fractum-start)))
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
Return the fractum’s first position, or nil if the buffer is empty.  See also
‘brec-at-fractum-start’ and ‘brec-in-fractum-start’.  For body fracta
and segments, see ‘brec-body-fractum-start’ and ‘brec-body-segment-start’."
  (let ((start (brec-body-fractum-start)))
    (unless (or start (= (point-min) (point-max)))
      ;; If point is not in the head of a body fractum, and the buffer is not empty,
      (setq start (point-min))); then point must be in the file head at its start.
    start))



(defvar brec-g); [GVF]



;;  brec-gap-pattern   (defined above in § Preliminary declarations)
(cl-assert (boundp 'brec-gap-pattern))



(defun brec-in-body-fractum-start ()
  "Whether point is on the start line of a body fractum.
Returns the fractum’s first non-space position, or nil if point is not
on the start line of a body fractum.  See also ‘brec-body-fractum-start’."
  (if (bolp)
      (brec-at-body-fractum-start)
    (save-excursion
      (beginning-of-line)
      (brec-at-body-fractum-start))))



(defun brec-in-body-segment-start ()
  "Whether point is on the start line of a body segment.
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



(defface brec-indent-blind-delimiter `((t . (:inherit brec-nobreak-space)))
  "The face for the no-break spaces that delimit an indent blind."
  :group 'brec-faces)



(defun brec-in-fractum-start ()
  "Whether point is on the start line of a fractum.
See also ‘brec-fractum-start’."
  (if (bolp)
      (brec-at-fractum-start)
    (save-excursion
      (beginning-of-line)
      (brec-at-fractum-start))))



(defun brec-is-divider-drawing (char)
  "Whether CHAR is a divider drawing character."
 (and (>= char ?\u2500) (<= char ?\u259F)))



(defun brec-is-divider-segment (segment-start)
  "Whether a body segment is a divider segment.
SEGMENT-START is the position of the segment’s first non-space character.
The return value is t if the body segment is a divider segment, nil otherwise."
  (brec-is-divider-drawing (char-after segment-start)))



(defun brec-is-divider-segment-successor (segment-start)
  "Whether a body segment is a divider segment that directly succeeds another.
SEGMENT-START is the position of the segment’s first non-space character.
The return value is the correponding position in the preceding divider segment,
or nil if the body segment is not a divider segment or has no divider-segment
predecessor.  See also ‘brec-is-divider-segment’ and
‘brec-divider-segment-predecessor’."
  (and (brec-is-divider-segment segment-start)           ; The segment is a divider segment
       (brec-divider-segment-predecessor segment-start))); and it directly succeeds another.



(defun brec-keywords ()
  "Return the value of ‘font-lock-keywords’ to use for highlighting Breccian text."
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
     '(brec--segment-end); (2, pre-form) Making the search region cover the whole of it. [REP]
     nil '(1 'brec-aside-descriptor)))



   ;; ═════════════
   ;; Command point
   ;; ═════════════

   (list; A command point starts with a perfectly indented (PI) bullet comprising one colon `:`.
    "^ \\{4\\}*\\(:\\) +[^ \n\\]"; (1) Anchoring on the bullet, initial space separator and first
    ;; ┈──────┘    character of the following term.  Assume that no term starts with a backslash,
    ;;    PI       so saving the cost of distinguishing it from a comment appender.

    '(1 'brec-command-bullet)

    ;; Descriptor
    ;; ──────────
    (list; (3, anchored highlighter) Always a descriptor follows the bullet,
     "\\(\\(?:.\\|\n\\)+\\)"; extending thence to the end of the point head.
     '(progn; (2, pre-form)
        (goto-char (match-end 1)); Starting from the end boundary of the bullet `:`.
        (setq
         brec-f (match-beginning 0)  ; Saving the start boundary of the present fractal segment
         brec-g (1- (match-end 0))   ; and the end boundaries both of the initial space separator
         brec-x (brec--segment-end))); and of the point head (N.B. this overwrites the match data),
           ;;; returning the latter and so extending the search region over the whole descriptor. [REP]
     nil '(1 'brec-command-descriptor))

    ;; Pattern matchers
    ;; ────────────────
    (list; (5, anchored highlighter)
     "\\(`\\)\\(\\(?:\\\\.\\|[^\n\\`]\\)+\\)\\(`\\)\\([isp]+\\)?"
     ;;  ╵           └────┘  └──────┘          ╵      └────┘     See `brec-pattern-matcher-pattern`
     ;;  Q             BC       NQ             Q         M       for a description of this pattern. [PMP]

     '(progn; (4, pre-form)
        (goto-char brec-g); Starting from the end boundary of the initial space separator,
        brec-x); again extend the search region over the whole descriptor.
     nil '(1 'brec-pattern-delimiter t) '(2 'brec-pattern t) '(3 'brec-pattern-delimiter t)
     '(4 'brec-pattern-match-modifier t t))

    ;; Context operators `@`
    ;; ─────────────────
    (list; (7, anchored highlighter)
     (lambda (limit)
       (defvar brec-preceding-gap-character-pattern); [FV]
       (defvar brec-succeeding-gap-character-pattern); [FV]
       (catch 'to-reface
         (while (re-search-forward
                 (concat brec-preceding-gap-character-pattern "\\(@\\)"
                         brec-succeeding-gap-character-pattern)
                 limit t)
           (let ((face (get-text-property (match-beginning 0) 'face)))
             (unless (eq face 'brec-pattern); Not to accept `@` characters that form pattern content.
               (throw 'to-reface t))))
         nil))
     '(progn; (6, pre-form)
        (goto-char brec-g); Starting from the end boundary of the initial space separator,
        brec-x); again extend the search region over the whole descriptor.
     nil '(1 'brec-command-operator t))

    ;; Appendage delimiter `:` and content
    ;; ───────────────────
    (list; (9, anchored highlighter)
     (lambda (limit)
       (defvar brec-preceding-gap-character-pattern); [FV]
       (defvar brec-succeeding-gap-character-pattern); [FV]
       (catch 'to-reface
         (when (re-search-forward
                 (concat brec-preceding-gap-character-pattern "\\(:\\)"
                         brec-succeeding-gap-character-pattern)
                 limit t)
           (let* ((m1-beg (match-beginning 0))
                  (m1-end (point))
                  (face (get-text-property m1-beg 'face)))
             (unless (eq face 'brec-pattern); Not to accept `:` characters that form pattern content.
               (set-match-data (list m1-beg limit
                                     m1-beg m1-end m1-end (goto-char limit) (current-buffer)))
               (throw 'to-reface t))))
         nil))
     '(progn; (8, pre-form)
        (goto-char brec-g); Starting from the end boundary of the initial space separator,
        brec-x); again extend the search region over the whole descriptor.
     nil '(1 'brec-command-operator t) '(2 'brec-command-appendage t))

    ;; Command (last that any `error` face it applies might override the foregoing)
    ;; ───────
    (list; (11, anchored highlighter)
     (mapconcat #'identity brec-command-matcher-components ""); Joining all components to one string.
     '(progn; (10, pre-form)
        (goto-char brec-f); Starting this time from the bullet `:` itself,
        brec-x); again extend the search region over the whole descriptor.
     nil
     '(1 'brec-command-operator t t) '(2 'brec-command-operator t t) '(3 'brec-command-operator t t)
     '(4 'error t t)))


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
              (setq brec-f match-beg; Saving the anchor’s bounds.
                    brec-g match-end)
              (throw 'to-anchor t))
            (setq match-beg match-end))
          nil)))
    (list; (3, anchored highlighter)
     (concat
      "\\(?:\\(?1:\\\\\\(?:[bdR]";   \b  \d  \R
         "\\|N{\\(?:[A-Z0-9 -]+";            \N{⋯}    (by name) [UCN]
            "\\|U\\+[0-9a-fA-F]+\\)}\\)\\)"; \N{U+⋯}  (by number)
      "\\|\\(?1:(\\(?:\\?:\\)?";             (  (?:
         "\\|\\$\\(?:{[^\n\\`]+?}\\)?";      $  ${⋯}  [PMP] though `\n` is redundant given `match-end`
         "\\|[.|)*+?^]+\\)";                 ^*  ^+  ^^  ^  .  |  )  *  +  ?
      "\\|\\(?2:\\\\[0-9[:alpha:]]\\)";      \·  (reserved backslash sequence)
      "\\|\\(?1:\\\\\\).";                   \·  (backslash-literal pair)
      "\\|\\(?2:[][{}]\\)\\)");        [ ]  { }  (reserved symbols)
     '(progn; (2, pre-form)
        (goto-char brec-f); To `match-beg` of the anchor effectively.
        brec-g); Limiting the search region (∵ return value is > point) effectively to `match-end`.
     nil '(1 'brec-pattern-element t t) '(2 'error t t)))



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
            '(brec--segment-end); (2, pre-form) Extending the search region over the whole segment. [REP]
            nil; (post-form)
            '(1 'brec-divider nil t);           `drawing-cap`
            '(2 'brec-titling-label nil t);     `titling-cap`
            '(3 'brec-division-label nil t)))); `labeling-cap`



   ;; ════════════════
   ;; Free-form bullet
   ;; ════════════════

   (list; Face each bullet of an alarm, task or plain point.
    (let ((rough-bullet-pattern; The best a regular expression can do here, allowing some false matches.
           (concat
            "^ \\{4\\}*\\("; Perfectly indented, the start of the bullet roughly comprises [CCP]
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
               ;;; (regular-expression pattern ` +\\+`) or a sequence of trailing space
               ;;; at end of the line (` +$`).  Trim it thus:
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
                        (setq brec-f 'brec-task-bullet-singleton)
                      (setq m2-end m1-end
                            m2-beg match-last
                            m1-end m2-beg
                            brec-f 'brec-task-bullet
                            brec-g 'brec-task-bullet-terminator
                            is-match-changed t))
                    (throw 'is-free-form-bullet t))

                  ;; Alarm bullet
                  ;; ────────────
                  (when (and (> length 1)
                             (= ?! char-last)
                             (= ?! (char-before match-last)))
                    (if (= length 2)
                        (setq brec-f 'brec-alarm-bullet-singleton)
                      (setq m2-end m1-end
                            m2-beg (1- match-last)
                            m1-end m2-beg
                            brec-f 'brec-alarm-bullet
                            brec-g 'brec-alarm-bullet-terminator
                            is-match-changed t))
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

                  ;; Plain bullet
                  ;; ──────────────
                  (setq brec-f 'brec-plain-bullet)
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
            (when (or (eq face 'brec-plain-bullet)
                      (eq face 'brec-task-bullet)
                      (eq face 'brec-alarm-bullet))
              (goto-char match-beg)
              (when (re-search-forward "[^[:alnum:] \u00A0]+" match-end t)
                (setq brec-f (intern (concat (symbol-name face) "-punctuation")))
                  ;;; To the punctuation variant of the face.
                (throw 'to-reface t)))
            (setq match-beg match-end))
          nil)))
    '(0 brec-f t))



   ;; ═══════════════
   ;; Comment carrier
   ;; ═══════════════

   (list; Fontify each line of a comment block.  A line may be marked by a single leading backslash (\),
      ;;; and may be followed by carried content (C) that begins with a space.
    "^ *\\(\\\\\\)\\(?:\\( +.*\\)?\\|\\(\\\\+\\)\\( +.*\\)?\\)$"; [CCP]
      ;;; └──────┘       └──────┘      └───────┘  └──────┘
      ;;;    \              C             \⋯         L

      ;;; Alternatively a leading backslash (\) may be followed by one or more further backslashes (\⋯),
      ;;; in which case any carried content constitutes a label (L).
    '(1 'brec-comment-block-delimiter t)   '(2 'brec-comment-block t t)
    '(3 'brec-comment-block-delimiter t t) '(4 'brec-comment-block-label t t))


   (list; Fontify each comment appender.  Each is delimited by any number of backslashes (\⋯)
      ;;; together isolated in whitespace.  Again, usually the delimiter is followed by content (C).
    (lambda (limit)
      (catch 'to-reface
        (while (re-search-forward "[^\n ] +\\(\\\\+\\)\\( +.*\\)?$" limit t); [CCP]
            ;;;                              └───────┘  └──────┘
            ;;;                                 \⋯        C

          (let ((face (get-text-property (match-beginning 1) 'face)))
            (unless (or (eq face 'brec-comment-block); Not naively to reface block commentary where it
                        (eq face 'brec-comment-block-label)); takes the superficial form of an appender.
              (throw 'to-reface t))))
        nil))
    '(1 'brec-comment-appender-delimiter t) '(2 'brec-comment-appender t t)); [OCA]



   ;; ══════════
   ;; Whitespace
   ;; ══════════

   (cons
    (lambda (limit)
      (let ((p (point))
            c face found)
        (while (and (not found) (< p limit))
          (setq c (char-after p))
          (cond

           ;; No-break space
           ;; ──────────────
           ((= c ?\u00A0)
            (setq face (get-text-property p 'face))
            (cond

             ;; In block commentary.
             ((and face (memq face '(brec-comment-block brec-comment-block-label)))
              (setq found t  brec-f 'brec-commentary-nobreak-space))

             ;; In a free-form bullet.
             ((and face (memq face '(brec-alarm-bullet brec-plain-bullet brec-task-bullet))); [NBB]
              (setq found t  brec-f 'brec-bullet-nobreak-space))

             ;; Delimiting an indent blind.
             ((brec-indent-before p)
              (setq found t  brec-f 'brec-indent-blind-delimiter))

             ;; Misplaced no-break space.
             (t (setq found t  brec-f 'brec-forbidden-whitespace))))

           ;; Forbidden character
           ;; ───────────────────
           ((or (memq c '(?\t ?\u202F ?\u205F ?\u3000))
                (and (>= c ?\u2000) (<= c ?\u200A)))
            (setq found t  brec-f 'brec-forbidden-whitespace))

           (t (setq p (1+ p)))))
        (when found
          (set-match-data (list p (goto-char (1+ p)) (current-buffer)))
          t))); Returning t to Font Lock if `found`, else nil.
    '(0 brec-f prepend)); Prepended only in case the original face is ever wanted.



   ;; ═════════════════════
   ;; Mathematic expression  [↑FF, ME]
   ;; ═════════════════════

   (list; Face mathematics by appending face `brec-math-inline` or `brec-math-block`.
    (let ((dd (concat (char-to-string brec-math-block-delimiter-char)
                      (char-to-string brec-math-inline-delimiter-char)))
          is-block match-beg match-end)
      (lambda (limit)
        (setq match-beg (point)); Presumptively.
        (catch 'to-reface
          (while (< match-beg limit)
            (setq match-end (next-single-property-change match-beg 'face (current-buffer) limit))
              ;;; Now mimic in the expression matcher that follows the behaviour of Breccia Web Imager
              ;;; (which never renders a math expression across a granal boundary) by restricting
              ;;; the matcher to the monoface sequence of text that ends at `match-end`. [↑FF]
            (when (re-search-forward
                   (concat "\\([" dd "]\\)\\(?:\\([^" dd "]+\\)\\(\\1\\)\\)?") match-end t)
              (setq is-block (eq (string-to-char (match-string 1)) brec-math-block-delimiter-char))
              (setq brec-f; The delimiter face.
                   (if (match-end 2)
                       (if is-block 'brec-math-block-delimiter nil)
                          ;;; For any delimiter of a pair that encloses a non-empty expression.
                     (if is-block 'brec-math-block-delimiter-error 'brec-transparent-error)))
                        ;;; For all other delimiters, whether of empty or open expressions.
              (setq brec-g; The expression face.
                   (if is-block 'brec-math-block 'brec-math))
              (throw 'to-reface t))
            (setq match-beg match-end)
            (goto-char match-beg))
          nil)))
    '(1 brec-f t) '(2 brec-g append t) '(3 brec-f t t))))



(defun brec--keyword-to-restore-line-spacing (original-value); [↑FF]
  (cons; Selectively restore (outside of indent blinds) the line spacing earlier zeroed.
   (lambda (limit)
     (let ((p (point))
           c face found in-blind)
       (while (and (not found) (< p limit))
         (setq c (char-after p))
         (cond
          ((= c ?\u00A0)
           (setq face (get-text-property p 'face))
           (when (and (listp face) (memq 'brec-indent-blind-delimiter face)); Here `face`
               ;;; would be a list, having been set using `prepend`, q.v. further above. [↑FF]
             (setq in-blind t)))
          ((= c ?\n)
           (if in-blind; Then keep going, seeking one that is outside a blind.
               (setq in-blind nil)
             (setq found t)))); Else (being outside a blind) restore the line spacing.
         (setq p (1+ p)))
       (when found; The character to fontify is just before `p`.
         (set-match-data (list (1- p) (goto-char p) (current-buffer)))
         t))); Returning t to Font Lock if `found`, else nil.
   `(0 (list  'face nil  'line-spacing ,original-value)))); [NF]



(defface brec-math `((t . (:inherit italic)))
  "The face for a mathematic expression.
See URL ‘http://reluk.ca/project/Breccia/Web/imager/bin/breccia-web-image.brec.xht#math’."
  :group 'brec-math-faces)



(defface brec-math-block `((t . (:inherit brec-math)))
  "The face for block-form (aka display) mathematics."
  :group 'brec-math-faces)



(defface brec-math-block-delimiter `((t . (:inherit brec-command-descriptor)))
  "The face for the delimiters of block-form mathematics."
  :group 'brec-math-faces)



(defface brec-math-block-delimiter-error `((t . (:inherit font-lock-warning-face :weight normal)))
  "The face for the delimiters of malformed (empty or open) block-form mathematics."
  :group 'brec-math-faces)



(defgroup brec-math-faces nil
  "Faces for mathematic expressions."
  :group 'brec-faces
  :prefix "brec-")



(defface brec-math-inline `((t . (:inherit brec-math)))
  "The face for in-line mathematics."
  :group 'brec-math-faces)



(defun brec-next-head (body-segment-start)
  "Locate the linear-order successor of a fractal head.
BODY-SEGMENT-START is the position of the first non-space character of
any body segment in the head, or nil for a file head.  The return value
is the correponding position in the next head, or nil if no next head exists."
  (when body-segment-start
    (let ((next (brec-next-segment body-segment-start)))
      (when (and next (brec-is-divider-segment body-segment-start))
        (while (and next (brec-is-divider-segment next))
          (setq next (brec-next-segment next))))
      next)))



(defun brec-next-segment (position)
  "Locate the linear-order successor of a fractal segment.
POSITION is any position within the segment.  The return value is the first
non-space character in the next segment, or nil if no next segment exists."
  (let (next)
    (save-excursion
      (goto-char position)
      (while (and (= 0 (forward-line))
                  (not (setq next (brec-at-body-segment-start))))))
    next))



(defun brec-next-sibling (body-segment-start)
  "Locate the next sibling of a fractum.
BODY-SEGMENT-START is the position of the first non-space character of any
body segment in the fractal head, or nil for a file head.  The return value is
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



(defun brec-next-sibling-or-elder (body-segment-start)
  "Locate the next sibling of a fractum, or of an ancestor of the fractum.
BODY-SEGMENT-START is the position of the first non-space character of any
body segment in the fractal head, or nil for a file head.  The return value
is the correponding position in the next fractum that is sibling of either
the fractum itself or an ancestor of the fractum, or nil if there is none."
  (when body-segment-start
    (let ((next-head body-segment-start)
          (sib-i (brec-indent-before body-segment-start))
          next-sib/elder)
      (while (and (setq next-head (brec-next-head next-head))
                  (not (when (<= (brec-indent-before next-head) sib-i); Found it.
                         (setq next-sib/elder next-head)))))
      next-sib/elder)))



(defface brec-nobreak-space `((t . (:inherit nobreak-space)))
  "The face for a no-break space (Unicode A0) in Breccia."
  :group 'brec-faces)



(defface brec-pattern `((t . (:inherit brec-command-descriptor)))
  "The face for a regular-expression pattern in the descriptor of a command point."
  :group 'brec-faces)



(defface brec-pattern-delimiter `((t . (:inherit brec-command-descriptor)))
  "The face for each of the delimiters of a regular-expression pattern."
  :group 'brec-faces)



(defface brec-pattern-element `((t . (:inherit brec-pattern)))
  "The face for a formal element of a regular-expression pattern."
  :group 'brec-faces)



;;  brec-pattern-matcher-pattern  (defined above in § Preliminary declarations)
(cl-assert (boundp 'brec-pattern-matcher-pattern))



(defface brec-pattern-match-modifier `((t . (:inherit brec-pattern-element)))
  "The face for a match modifier of a regular-expression pattern."
  :group 'brec-faces)



(defface brec-plain-bullet `((t . (:inherit (brec-bullet font-lock-keyword-face))))
  "The face for the bullet of a plain point."
  :group 'brec-point-faces)



(defface brec-plain-bullet-punctuation `((t . (:inherit brec-plain-bullet)))
  "The face for non-alphanumeric characters in the bullet of a plain point."
  :group 'brec-point-faces)



(defgroup brec-point-faces nil
  "Faces for points."
  :group 'brec-faces
  :prefix "brec-")



(defconst brec-preceding-gap-character-pattern "[ \n]"
  "The regex pattern of a gap character that could directly precede a non-gap.
See also ‘brec-gap-pattern’.");



(defun brec-previous-body-segment (position)
  "Locate the linear-order body-segment predecessor of a fractal segment.
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
  "Locate the linear-order predecessor of a fractal head.
START-SEGMENT-POSITION is any position in the first body segment of the head,
or nil for a file head.  The return value is the position of the first
non-space character in the previous head, or nil if no previous head exists."
  (when start-segment-position
    (let ((previous (brec-previous-body-segment start-segment-position))
          p)
      (when previous
        (while (setq p (brec-is-divider-segment-successor previous))
          (setq previous p)))
      previous)))



(defun brec-previous-sibling (body-fractum-start)
  "Locate the previous sibling of a fractum.
BODY-FRACTUM-START is the position of the fractum’s first non-space character,
or nil for the file fractum.  The return value is the correponding position
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



(defun brec-segment-end ()
  "The position at the end of the last line of the present fractal segment.
Point must not lie at the start of a body segment, or the result is undefined.
See also ‘brec-body-segment-start-pattern’ and ‘brec--segment-end’."
  (save-excursion
    (when (bolp)   ; Then ‘brec-body-segment-start-pattern’ might (below) match the present segment;
      (end-of-line)); but this far at least the present segment extends, and moving to here
                   ;;; prevents that error.
    ;; Changing what follows?  Sync → `brec--segment-end`.
    (if (re-search-forward brec-body-segment-start-pattern nil t); Cf. `brec-extend-search-down`.
        (end-of-line 0); Moving to the end of the previous line.
      (goto-char (point-max)))
    (point)))



(defun brec--segment-end ()
  "Like ‘brec-segment-end’, except with one proviso as follows.
Point must not lie at the start of a body segment or the result is undefined."
  (save-excursion; Changing what follows?  Sync → `brec-segment-end`.
    (if (re-search-forward brec-body-segment-start-pattern nil t); Cf. `brec-extend-search-down`.
        (end-of-line 0); Moving to the end of the previous line.
      (goto-char (point-max)))
    (point)))



(defun brec-set-for-buffer (variable value)
  "Set buffer-local VARIABLE (a symbol) to VALUE.
Signal an error if the binding is not actually buffer-local.
This might happen, for example, if an externally defined VARIABLE
that was documented as being buffer-local no longer is."
  (set variable value)
  (cl-assert (local-variable-p variable)))



(defconst brec-succeeding-gap-character-pattern "[ \n]"
  "The regex pattern of a gap character that could directly follow a non-gap.
See also ‘brec-gap-pattern’.");



(defface brec-task-bullet
  `((t . (:inherit (brec-bullet font-lock-function-name-face))))
  "The face for the bullet of a task point."
  :group 'brec-point-faces)



(defface brec-task-bullet-punctuation `((t . (:inherit brec-task-bullet)))
  "The face for a non-alphanumeric character of a task bullet.
Cf. ‘brec-task-bullet-singleton’ and ‘brec-task-bullet-terminator’."
  :group 'brec-point-faces)



(defface brec-task-bullet-singleton `((t . (:inherit brec-task-bullet)))
  "The face for a task bullet that comprises \\=`+\\=` alone."
  :group 'brec-point-faces)



(defface brec-task-bullet-terminator `((t . (:inherit font-lock-comment-face)))
  "The face for the bullet terminator \\=`+\\=` of a non-singleton task point.
Cf. ‘brec-task-bullet-singleton’."
  :group 'brec-point-faces)



;;  brec-term-end-boundary-pattern  (defined above in § Preliminary declarations)
(cl-assert (boundp 'brec-term-end-boundary-pattern))



(defface brec-titling-label `((t . (:inherit (bold brec-division-label))))
  "The face for a division label that contributes to the division title, or titles."
  :group 'brec-faces)


(defcustom brec-to-collapse-indent-blinds nil
  "Whether to collapse the line spacing of indent blinds to zero.
When t, any ‘line-spacing’ in effect for the buffer is zeroed then selectively
restored outside of indent blinds using the \\=`line-spacing\\=` text property.
This can be useful to enable seamless jointing of semigraphics,
such as box-drawing characters, within indent blinds.

Note however that the line-spacing restoration *outside* of indent blinds
works only for lines that fit the width of the display window.  Longer lines
(which Emacs truncates) will get the same (zero) line spacing as indent blinds.
This can be annoying.  Consider limiting the use of this option to just
those files that need it by setting it as a file variable."
  :group 'brec
  :link '(url-link
          "https://www.gnu.org/software/emacs/manual/html_node/emacs/Specifying-File-Variables.html")
  :safe #'booleanp
  :type 'boolean)



(defface brec-transparent-error `((t . (:inherit font-lock-warning-face :inverse-video t)))
  "An error face for characters whose glyphs are normally transparent, such as whitepace."
  :group 'brec-faces)



(defvar brec-x); [GVF]



;; ══════════════════════════════════════════════════════════════════════════════════════════════════════
;;  P a c k a g e   p r o v i s i o n
;; ══════════════════════════════════════════════════════════════════════════════════════════════════════


;;;###autoload (set 'auto-mode-alist (cons (cons "\\.brec\\'" 'brec-mode) auto-mode-alist))



;;;###autoload
(define-derived-mode brec-mode text-mode
  "Breccia"
  "A major mode for editing Breccia.
Breccia is a lightweight markup language for point-form outlining and drafting.
For more information, see URL ‘http://reluk.ca/project/Breccia/’
and URL ‘http://reluk.ca/project/Breccia/Emacs/’."
  :group 'brec
  :after-hook (progn


    ;; ═══════════════
    ;; II. Late set-up
    ;; ═══════════════

    ;; Seamless jointing of semigraphics in indent blinds
    ;; ─────────────────────────────────
    (when brec-to-collapse-indent-blinds
      (let ((s (or line-spacing (frame-parameter nil 'line-spacing))))
        (when (and s (/= 0 s)); Text properties can enlarge line spacing only, they cannot zero it.
          (brec-set-for-buffer 'line-spacing 0); Therefore zero it for the whole buffer, then
            ;;; selectively restore it outside of indent blinds using the namesake text property:
          (make-local-variable 'font-lock-extra-managed-props)
          (add-to-list 'font-lock-extra-managed-props 'line-spacing)
          (font-lock-add-keywords
           nil (list (brec--keyword-to-restore-line-spacing s)) 'append))))); [↑FF]



  ;; ═══════════════
  ;; I. Early set-up
  ;; ═══════════════

  (let ((b (char-to-string brec-math-block-delimiter-char))
        (i (char-to-string brec-math-inline-delimiter-char)))

    ;; Character syntax
    ;; ────────────────
    (let ((s brec-mode-syntax-table)
          (sb (concat "$" b))
          (si (concat "$" i)))
      (modify-syntax-entry        ?\' "$'"  s); Paired-delimiter syntax on ASCII single
      (modify-syntax-entry        ?\" "$\"" s); and double quotes,
      (modify-syntax-entry        ?`  "$`"  s); regular-expression pattern delimiters,
      (modify-syntax-entry
       brec-math-block-delimiter-char  si   s); plus the delimiters for in-line
      (modify-syntax-entry                  ;;; and block-form mathematics.
       brec-math-inline-delimiter-char sb   s)
  ;;; (modify-syntax-entry        ?\‘ "$’"  s); ← The same syntax on symmetric single quotes, however,
  ;;; (modify-syntax-entry        ?\’ "$‘"  s);   fails to enable `forward-sexp` between each pair
  ;;; (modify-syntax-entry        ?\“ "$”"  s); ← (though oddly it succeeds on double quotes).
  ;;; (modify-syntax-entry        ?\” "$“"  s);   Therefore do the following instead.
      (modify-syntax-entry        ?\‘ "(’"  s); Parenthetic syntax
      (modify-syntax-entry        ?\’ ")‘"  s); on symmetric single
      (modify-syntax-entry        ?\“ "(”"  s); and double quotes.
      (modify-syntax-entry        ?\” ")“"  s))

    ;; Font Lock integration
    ;; ─────────────────────
  ;;; (brec-set-for-buffer 'font-lock-multiline t)
  ;;;;;;; It seems unnecessary and the description for `font-lock-multiline` does not imply otherwise.
      ;;; It might become necessary if fontification ever demands a rapid response to changes
      ;;; on subsequent lines.  Meantime it seems `brec-extend-search` alone suffices:
    (add-hook 'font-lock-extend-region-functions #'brec-extend-search t t)
      ;;; The alternative to `font-lock-extend-region-functions`, namely the little used
      ;;; `font-lock-extend-after-change-region-function`, appears to be a design error.
      ;;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2015-03/msg00818.html
    (brec-set-for-buffer 'font-lock-defaults '(brec-keywords))

    ;; `math-preview` integration
    ;; ──────────────────────────
    (when (package-installed-p 'math-preview)
      (setq-local math-preview-tex-marks        (list (list b b 0 nil nil))
                  math-preview-tex-marks-inline (list (list i i 0 nil nil)))))

  ;; No-break-space display (Unicode A0)
  ;; ──────────────────────
  (setq-local nobreak-char-display nil); Default application of standard face `nobreak-space`. [SF]
     ;;; Defeat it, because it applies the face by a method unamenable to override in `brec-keywords`.
     ;;; Instead let Brec Mode face these characters using standard, Font Lock methods.

  ;; Paragraph handling: detection, filling and transit among fracta and fractal heads as “paragraphs”
  ;; ──────────────────
  (setq-local
   adaptive-fill-regexp
     " *\\([[:alnum:]]?[-+/:.′″…⋮⋯∅∞×=≠∼)∵∴∃¶§'`\"?¿$¢°–―!‼|#%;>*†‡·•‣▹▸▷▶⁃◦○◌⋅∙]+ *\\)*"; This at least
       ;;; works for most bullets of one or two characters length, though it could be better expressed,
       ;;; or even better generalized to cover all forms of bullet.
   paragraph-start brec-body-segment-start-pattern; [PBD]
   paragraph-separate "^ *\\(?:\u00A0.*\\|\\\\+\\( +.*\\)?\\)?$"); [CCP, PBD]
     ;;; Indent blinds, comment blocks and blank lines, that is.
  (let ((m brec-mode-map))
    (define-key m [remap backward-paragraph] #'brec-backward)
    (define-key m [remap forward-paragraph] #'brec-forward)))



(provide 'brec-mode)


;; NOTES
;; ─────
;;   BUG  This code is incorrect.
;;
;;   CCP  Comment-carriage pattern.  Marking an instance of a pattern or anti-pattern related to
;;        comment carriers, one of multiple instances that together are maintained in synchrony.
;;
;;  ↑FF · Fontification code that must execute late in the fontification sequence.
;;
;;   FV · Suppressing sporadic compiler warnings ‘reference to free variable’
;;        or ‘assignment to free variable’.
;;
;;   GVF  A global variable for the use of fontifiers, e.g. from within forms they quote and pass
;;        to Font Lock to be evaluated outside of their lexical scope.
;;
;;   LLT  Busy loops owing to line terminator in `brec-gap-pattern`.
;;            (1) Use of `$` has caused busy looping, and `\n?` was the repair.
;;        https://github.com/Michael-Allan/Breccia.Emacs/commit/fec92482f6c3bb1a859792dcec409bc4f3264763
;;            (2) Now `\n?` itself is the cause and the original `$` the repair (2021-7, Emacs 27.2).
;;
;;   ME · Breccia Web Imager includes an option to render mathematic expressions.
;;        http://reluk.ca/project/Breccia/Web/imager/bin/breccia-web-image.brec.xht#math
;;
;;   NBB  No-break space in a bullet.  Alone a face test suffices to guard the application
;;        of `brec-bullet-nobreak-space` only because already the bullet fontifier detects
;;        and refuses to face misplaced no-break spaces as bullet constituents.  For more on this,
;;        see `http://reluk.ca/project/Breccia/Emacs/notes.brec.xht` § no-break spaces.
;;
;;   NCE  Not `char-equal` or `=`, which fail if the position is out of bounds.
;;        Rather `eq` which instead gives nil in that case.
;;
;;   NF · In a search-based fontifier, a nil value for the `face` property of a list-form *facespec*
;;        has the (undocumented) effect of leaving the face property untouched at its present value.
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
;;
;;   OCA  Overrides in comment-appender fontification.  The fontifier must override (t) any facing
;;        of the appender’s containing head, and must therefore follow it in `brec-keywords`.
;;            Maybe the syntax system, which runs earlier, would suffice for fontifying comment carriers.
;;        Not mere syntax tabulation, which would be unable to grasp their form.  Rather the macro
;;        `syntax-propertize-rules`, which might set the necessary syntax properties on the delimiters.
;;        But then, in the case of a comment appender, could the `subexp-highlighters` of the containing
;;        head have worked around the carrier, e.g. with `override` at nil?  [SBF]
;;
;;   PBD  Paragraph boundary definition.  It is used by the command `fill-paragraph`, for instance
;;        (though not by the Breccian equivalents of `backward-paragraph` and `forward-paragraph`,
;;        namely `brec-backward` and `brec-forward`, to which the keys of the former are remapped).
;;            The manual says it “should not use `^` to anchor the match”; yet without that anchor,
;;        `fill-paragraph` fails, instead collapsing each paragraph (fractal head) to a single line.
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Regexps.html
;;
;;   PMP  Pattern-matcher pattern.  Marking an instance of a pattern related to pattern matchers,
;;        one of multiple instances that together are maintained in synchrony.
;;
;;   PSA  Plain spaces alone are valid separators here, yet the pattern should not be broken
;;        merely on account of forbidden whitespace.  Forbidden whitespace is the separate
;;        concern of a dedicated fontifier.  See code § Whitespace.
;;
;;   REP  Region extension in the pre-form of an anchored highlighter: extending the end boundary
;;        of the search region for multi-line fontification.  The manual warns, ‘It is generally
;;        a bad idea to return a position greater than the end of the line’ [SBF].
;;        But here the manual appears to be wrong.  https://stackoverflow.com/a/9456757/2402790
;;
;;   SBF  Search-based fontification.
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
;;
;;   SF · Standard faces: `https://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html`
;;        and § Standard faces at `http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/faces.el`.
;;
;;   UCN  Unicode character name. https://en.wikipedia.org/wiki/Unicode_character_property#Name


;;; brec-mode.el ends here
