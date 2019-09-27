;; The definition of Breccia mode, a major mode for editing Breccian text.
;;
;; USAGE
;; ─────
;;   In your initialization file:
;;
;;      (require 'breccia-mode)
;;      (add-to-list 'auto-mode-alist '("\\.brec\\'" . breccia-mode))
;;
;;   Working example:
;;
;;       http://reluk.ca/.emacs
;;       http://reluk.ca/.Xresources
;;       http://reluk.ca/project/Breccia/Emacs/screen_shot.png
;;
;;
;; TERMS
;; ─────
;;   fontification segment
;;       One of a document head, point head or divider segment.
;;
;;
;; NOTES  (see at bottom)
;; ─────


(define-derived-mode breccia-mode text-mode
  "Breccia" "A major mode for editing Breccian text"
  (modify-syntax-entry ?\u00A0 " " breccia-mode-syntax-table); Giving to no-break spaces (Unicode A0)
  (set (make-local-variable 'nobreak-char-display) t)        ; whitespace syntax, and a distinct look
     ;;; as defined by the Emacs standard face `nobreak-space`. [SF]

  ;; Define paragraph bounds, e.g. for sake of the `fill-paragraph` command
  ;; ───────────────────────
 ;(set 'use-hard-newlines t); It says, ‘Automatically becomes permanently buffer-local when set.’
 ;;; Unexpectedly that wrecks rather than helps the following.
  (set (make-local-variable 'paragraph-start) (concat brec-seg-start-pattern ".*$"))
  (set (make-local-variable 'paragraph-separate) "^ *\\(?:\u00A0.*\\|\\\\+\\( +.*\\)?\\)?$")
    ;;; Blank lines, indentation blinds and block commentary, that is.

  ;; Set up font-lock
  ;; ────────────────
 ;(set (make-local-variable 'font-lock-multiline) t)
 ;;; This setting does not, however, seem necessary; nor does the documentation imply that it would be.
 ;;; Should fontification ever depend on *subsequent* lines, there I think this setting would at least
 ;;; speed the response to changes.  Meantime, it seems that `brec-extend-search` alone will suffice:
  (add-hook 'font-lock-extend-region-functions 'brec-extend-search) ; [FLE]
  (set 'font-lock-defaults '(brec-keywords))); ‘It automatically becomes buffer-local when set.’ [FLB]



;; ══════════════════════════════════════════════════════════════════════════════════════════════════════
;;  P r e l i m i n a r y   d e c l a r a t i o n s
;; ══════════════════════════════════════════════════════════════════════════════════════════════════════


(defconst brec-gap-pattern "[ \n]+"; This is incomplete, it omits commentary and indentation blinds [D].
  "The regexp pattern of a gap in a descriptor.")



(defconst brec-backquoted-pattern-pattern "`\\(?:\\\\.\\|[^\\`]\\)+`"
  ;;                                       ╵     └────┘  └────┘    ╵
  ;;                                       Q       BC      NQ      Q
  ;;
  ;; Each element between the backquotes (Q) is either a blackslashed character pair (BC) such as “\n”
  ;; or “\`”, or a single, non-backquote character (NQ).

  "The regexp pattern of a regexp pattern in backquotes.")



;; ══════════════════════════════════════════════════════════════════════════════════════════════════════
;;  D e c l a r a t i o n s   i n   l e x i c a l   o r d e r
;; ══════════════════════════════════════════════════════════════════════════════════════════════════════


(defface brec-alarm-bullet-face
  `((default . (:inherit (brec-bullet-face font-lock-warning-face))))
  "The face for the bullet of an alarm point.")



(defface brec-alarm-bullet-terminator-face
  `((default . (:inherit brec-alarm-bullet-face))
    (t :weight normal))
  "The face for the bullet terminator ‘!!’ of an alarm point.")



(defface brec-aside-bullet-face
  `((default . (:inherit (brec-bullet-face brec-aside-descriptor-face))))
  "The face for the bullet of an aside point.")



(defface brec-aside-descriptor-face
  `((default . (:inherit shadow)))
  "The face for the descriptor of an aside point.")



(defface brec-bullet-face
  `((default . (:inherit bold)))
  "The face for a bullet.")



(defface brec-command-keyword-face
  `((default . (:inherit brec-command-descriptor-face)))
  "The face for a keyword in the descriptor of a command point.")



(defface brec-command-bullet-face
  `((default . (:inherit (brec-bullet-face brec-command-descriptor-face))))
  "The face for the bullet of a command point.")



(defface brec-command-descriptor-face
  `((default . (:inherit font-lock-builtin-face)))
  "The face for the descriptor of a command point.")



(defvar brec-command-highlighter-components; Components of an `anchored-highlighter`, that is.
  (let ((bq-pat brec-backquoted-pattern-pattern)
        (gap brec-gap-pattern)); Not having commentary or indentation blinds in `brec-gap-pattern` (q.v.)
          ;;; may cause the highlighter to fail in some texts, leaving a command unhighlighted. [BUG]
          ;;; An obvious workaround for a user editing the text is to pull commentary and indentation
          ;;; blinds out of the command and put them instead at the end of the descriptor.
    (list
     (concat ":" gap "\\(?:"); (initial component)

     ;; Associative reference
     ;; ─────────────────────
     (concat
      "\\(?:\\(?1:re\\)" gap bq-pat gap "\\)?"; Referrer clause.

      ;; Referent clause.  It determines the specific type of associative reference as either
      "\\(?2:join"  ; a jointer or
      "\\|see\\)\\>"); a pointer.

     ;; Other commands
     ;; ──────────────
     ;; Components for new command patterns may be inserted here.  Each pattern must open with "\\|"
     ;; and may capture up to two, explicitly numbered groups, e.g. `\(?1:foo\)` and `\(?2:bar\)`.
     ;; The same fontification will be given to both groups.  For an example of component insertion,
     ;; see `http://reluk.ca/project/wayic/Waybrec/Emacs/waybrec-mode.el`.

     "\\)")); (final component)

  "The list of components (each a string) that function `brec-keywords` will concatenate in order
to form the `anchored-highlighter` it uses to fontify the command portion of each command point.
Derived modes may modify the list before calling `brec-keywords`, e.g. by inserting components
in order to fontify additional commands.  Developers should read the instructions in the source code
of `brec-command-highlighter-components` before attempting to do that.")



(defface brec-comment-block-label-face
  `((default . (:inherit font-lock-doc-face)))
  "The face for a comment block label.")



(defface brec-divider-face
  `((default . (:inherit font-lock-doc-face)))
  "The face for a divider.")



(defface brec-division-inverse-labeling-face
  `((default . (:inherit (bold brec-divider-face)))
    (t :inverse-video t))
  "The face for inverse labeling and reverse video in a division label.")



(defface brec-division-label-face
  `((default . (:inherit brec-divider-face)))
  "The face for a label in a divider.")



(defface brec-division-titling-face
  `((default . (:inherit (bold brec-division-label-face))))
  "The face for a titling sequence in a division label.")



(defun brec-extend-search()
  "Ensures that the font-lock search region extends to cover the whole of its fontification segments,
bisecting none of them.  Returns nil if already it does, non-nil otherwise."
  (save-excursion
    (let ((is-changed (brec-extend-search-up)))
      (or (brec-extend-search-down) is-changed))))



(defun brec-extend-search-down()
  "Ensures that `font-lock-end` bisects no fontification segment, moving it forward in the buffer
as necessary.  Returns nil if no change was required, non-nil otherwise."
  (goto-char font-lock-end)
  (when (not (or (bolp)(eolp))) ; When the prior extenders such as `font-lock-extend-region-wholelines`
    ;; do not leave `font-lock-end` at a line terminus, as usually they do, then the search
    ;; region bisects the text of the line, which means the text of a fontification segment
    ;; (a Breccian document contains nothing else), and each segment covers the whole of its lines.
    (end-of-line)) ; Thus far at least the present segment must extend; extend it now,
                 ;;; that `re-search-forward` (below) must miss its leader.
  (let (is-changed)
    (if (re-search-forward brec-seg-start-pattern nil t); Cf. `brec-seg-end`.
        (end-of-line 0); Moving to the end of the previous line.
      (goto-char (point-max)))
    (when (< font-lock-end (point))
      (set 'font-lock-end (point))
      (set 'is-changed t))
    is-changed))



(defun brec-extend-search-up()
  "Ensures that `font-lock-beg` bisects no fontification segment, moving it backward in the buffer
as necessary.  Returns nil if no change was required, non-nil otherwise."
  (goto-char font-lock-beg)
  (end-of-line); That `re-search-backward` (below) finds any leader on the present line.
  (let (is-changed)
    (if (re-search-backward brec-seg-start-pattern nil t)
        (beginning-of-line)
      (goto-char (point-min)))
    (when (> font-lock-beg (point))
      (set 'font-lock-beg (point))
      (set 'is-changed t))
    is-changed))



(defface brec-forbidden-whitespace-face
  `((default . (:inherit font-lock-warning-face))
    (t :inverse-video t))
  "The face for disallowed, horizontal whitespace characters.")



(defface brec-generic-bullet-face
  `((default . (:inherit (brec-bullet-face font-lock-keyword-face))))
  "The face for the bullet of a generic point.")



(defface brec-generic-bullet-punctuation-face
  `((default . (:inherit brec-generic-bullet-face))
    (t :weight normal))
  "The face for non-alphanumeric characters in the bullet of a generic point.")



(defun brec-keywords()
  "Returns the value of `font-lock-keywords` to use for highlighting Breccian text."
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
         (inversion-iii (concat "\\(" inversion-mark "\\)\\( *\\(?:" labeling " *\\)?\\)\\(\u2588\\)?"))
           ;;; Capturing (i) an inversion mark, (ii) any `labeling` together with any space characters
           ;;; around it, and (iii) any full block character.
         region-start region-end)
    (list

     ;; ═══════════
     ;; Aside point
     ;; ═══════════
     ;; An aside point starts with a perfectly indented (PI) bullet comprising one slash (/).
     (list "^ \\{4\\}*\\(/\\)\\(?: +\\|$\\)"
           ;; ┈──────┘
           ;;    PI

           '(1 'brec-aside-bullet-face)

           (list                     ; Usually a descriptor follows the bullet,
            "\\(\\(?:.\\|\n\\)+\\)"  ; extending thence to the end of the point head.
            '(brec-seg-end); `pre-form`: Making the search region cover the whole of it. [PSE]
            nil '(1 'brec-aside-descriptor-face)))


     ;; ═════════════
     ;; Command point
     ;; ═════════════
     ;; A command point starts with a perfectly indented (PI) bullet comprising one colon (:).
     (list "^ \\{4\\}*\\(:\\)\\(?: +\\|$\\)"
           ;; ┈──────┘
           ;;    PI

           '(1 'brec-command-bullet-face)

           ;; Descriptor
           ;; ──────────
           (list; `anchored-highlighter`: Usually a descriptor follows the bullet,
            "\\(\\(?:.\\|\n\\)+\\)";      extending thence to the end of the point head.
            '(funcall; `pre-form`
              (lambda()
                (set 'region-start (point))     ; For later recall.  Now return `brec-seg-end`, making
                (set 'region-end (brec-seg-end)))); the search region cover the whole descriptor. [PSE]
            '(funcall (lambda() (goto-char region-start))); `post-form`: Clean-up for next highlighter.
            '(1 'brec-command-descriptor-face))

           ;; Command
           ;; ───────
           (list
            (mapconcat 'identity brec-command-highlighter-components ""); Concatenating all the
            '(funcall; `pre-form`                                         components to one string.
              (lambda()
                (while (progn (backward-char)                     ; Bringing the bullet ‘:’
                              (not (char-equal ?: (char-after))))); into the search region
                region-end)); and (again) ensuring it extends to the end of the descriptor.
            nil '(1 'brec-command-keyword-face t t) '(2 'brec-command-keyword-face t t)))


     ;; ═══════
     ;; Divider
     ;; ═══════
     ;; A divider starts with a perfectly indented (PI) sequence of drawing or inversion.
     (list
      (concat "^ \\{4\\}*\\(?:" drawing-i "\\|" inversion-iii "\\)")
      ;;       └────────┘
      ;;            PI

      '(1 'brec-divider-face nil t); `drawing-i`
      '(2 'brec-divider-face nil t)                  ; i,
      '(3 'brec-division-inverse-labeling-face nil t); ii and
      '(4 'brec-divider-face nil t)                  ; iii of `inversion-iii`.

      ;; Thence it may include any mix of drawing, titling, labeling and inversion sequences.
      (list (concat drawing-i "\\|" titling-i "\\|" labeling-i "\\|" inversion-iii)
            '(brec-seg-end); `pre-form`: Making the search region cover a whole segment of it. [PSE]
            nil; `post-form`
            '(1 'brec-divider-face nil t);          `drawing-i`
            '(2 'brec-division-titling-face nil t); `titling-i`
            '(3 'brec-division-label-face nil t);  `labeling-i`
            '(4 'brec-divider-face nil t)                  ; i,
            '(5 'brec-division-inverse-labeling-face nil t); ii and
            '(6 'brec-divider-face nil t)))                ; iii of `inversion-iii`.


     ;; ════════════════
     ;; Free form bullet of an alarm, task or generic point
     ;; ════════════════
     (list
      (lambda( limit ); Seek the next such bullet.
        (catch 'result
          (while (re-search-forward; Start with a naive search, the best a regexp can do here.
                  (concat
                   "^ \\{4\\}*\\(\\\\*"; Perfectly indented (PI), the bullet starts with
                   ;; ┈──────┘   └───┘   zero or more backslashes (\⋯) and a character
                   ;;    PI        \⋯    that is neither whitespace nor a backslash:
                   ;;
                   "\\(?:[[:alnum:]]+ *\\|[^[:alnum:][:space:]\\][\u00A0]*\\)"

                   ;; Thence it continues.  It ends before a space that comes immediately after
                   ;; a non-alphanumeric, non-space character; or it ends before a newline.
                   ;; (Notably, a no-break space (Unicode A0) that comes immediately after
                   ;; a non-alphanumeric, non-space character does *not* end the bullet.)
                   "\\(?:[[:alnum:]]+ *\\|[^[:alnum:][:space:]]+[\u00A0]*\\)*\\)")

                  limit t)
            (let ((m1-beg (match-beginning 1)); Now ensure the resulting (naive) match is correct.
                  (m1-end (match-end 1))
                  m2-beg m2-end m3-beg m3-end m4-beg m4-end m5-beg m5-end is-match-changed)

              (let ((end m1-end)); Trim from the match any unwanted end boundary missed above.
                 ;; It is either a delimiter of inline commentary (regexp pattern ‘ +\\+’)
                 ;; or a sequence of trailing space at the line end (‘ +$’).  Trim it thus:
                (while (char-equal (char-before end) ?\\); For any trailing backslashes captured,
                  (set 'end (1- end)))                   ; scan backward past them.
                (while (char-equal (char-before end) ?\s); For any trailing space characters,
                  (set 'end (1- end))                    ; scan backward past them, and trim
                  (set 'm1-end end)                       ; the whole from the captive group.
                  (set 'is-match-changed t)))
              (when
                  (catch 'is-matched
                    (let ((char-last (char-before m1-end)))
                      (when (char-equal ?+ char-last); If a task bullet is captured,
                        (set 'm2-beg m1-beg)          ; then recapture it as follows.
                        (if (= 1 (- m1-end m1-beg))   ; If it comprises ‘+’ alone,
                            (set 'm2-end m1-end)      ; then recapture it as group 2.
                          (set 'm2-end (- m1-end 1))  ; Else it has a ‘body’, too.
                          (set 'm3-beg m2-end)        ; Recapture its body as group 2
                          (set 'm3-end m1-end))       ; and its ‘+’ terminator as group 3.
                        (set 'm1-beg nil)
                        (set 'm1-end nil)
                        (set 'is-match-changed t)
                        (throw 'is-matched t))

                      (let ((length (- m1-end m1-beg)))
                        (when (and (> length 1)     ; If an alarm bullet is captured,
                                   (char-equal ?! char-last)
                                   (char-equal ?! (char-before (1- m1-end))))
                          (set 'm4-beg m1-beg)        ; then recapture it as follows.
                          (if (= 2 (- m1-end m1-beg)) ; If it comprises ‘!!’ alone,
                              (set 'm4-end m1-end)    ; then recapture it as group 4.
                            (set 'm4-end (- m1-end 2)); Else it has a ‘body’, too.
                            (set 'm5-beg m4-end)      ; Recapture its body as group 4
                            (set 'm5-end m1-end))     ; and its ‘!!’ terminator as group 5.
                          (set 'm1-beg nil)
                          (set 'm1-end nil)
                          (set 'is-match-changed t)
                          (throw 'is-matched t))

                        (let ((char-first (char-after m1-beg)))
                          ;; Abandon any unwanted match — of either a non-bullet (divider) or a bullet
                          ;; of tightly constrained form (aside point or command point) — as follows.
                          (when (and (= 1 length); If exactly one character is captured and it is
                                     (or (char-equal ?/ char-first)  ; either an aside bullet
                                         (char-equal ?: char-first))); or command bullet,
                            (throw 'is-matched nil)); then abandon the match and continue seeking.

                          (when (and (>= char-first ?\u2500) ; If a divider mark leads the match,
                                     (<= char-first ?\u259F)); then abandon it and continue seeking.
                            (throw 'is-matched nil)))))
                    t)
                (when is-match-changed
                  (set-match-data
                    (list (match-beginning 0) (match-end 0) m1-beg m1-end
                          m2-beg m2-end m3-beg m3-end
                          m4-beg m4-end m5-beg m5-end
                          (current-buffer))))
                (throw 'result t))))
          (throw 'result nil)))
      '(1 'brec-generic-bullet-face nil t)
      '(2 'brec-task-bullet-face nil t) '(3 'brec-task-bullet-terminator-face nil t)
      '(4 'brec-alarm-bullet-face nil t) '(5 'brec-alarm-bullet-terminator-face nil t))

     (cons; Refontify the non-alphanumeric characters of generic bullets.
      (lambda( limit )
        (catch 'result
          (while (< (point) limit)
            (let ((face (get-text-property (point) 'face))
                  (face-limit (next-single-property-change (point) 'face (current-buffer) limit)))
              (when (and (eq face 'brec-generic-bullet-face)
                         (re-search-forward "[^[:alnum:] \u00A0]+" face-limit t))
                (throw 'result t))
              (goto-char face-limit)))
          (throw 'result nil)))
      '(0 'brec-generic-bullet-punctuation-face t))


   ;;; ──  D e f e r r e d   f o n t i f i c a t i o n  ─────────────────────────────────────────────────

     ;; ══════════
     ;; Commentary
     ;; ══════════
     ;; Commentary is delimited per line by one or more backslashes (\⋯) together isolated
     ;; in whitespace.  Usually the delimiter is followed by commentary content (C) too.
     (list "\\(?:^\\| \\)\\(\\\\+\\)\\( +.*\\)?$"; [RWC, SPC]
           ;;              └───────┘  └──────┘
           ;;                  \⋯         C

           '(1 'font-lock-comment-delimiter-face t) '(2 'font-lock-comment-face t t)); [OCF]

     ;; Moreover where a line of pure commentary is delimited by two or more backslashes (\\⋯),
     ;; any content is taken to be a block label (L).
     (cons "^ *\\\\\\{2,\\}\\( +.+\\)$" '(1 'brec-comment-block-label-face t)); [OCF, RWC, SPC]
       ;;;     └──────────┘  └──────┘
       ;;;         \\⋯           L


     ;; ════════════════════
     ;; Forbidden whitespace
     ;; ════════════════════
     (cons "[\t\u2000-\u200A\u202F\u205F\u3000]" '(0 'brec-forbidden-whitespace-face t)))))
       ;;;    9, 2000 - 200A, 202F, 205F, 3000
       ;;;
       ;;; No attempt is made here to fontify any no-break spaces (Unicode A0) that appear
       ;;; in forbidden contexts.  They will at least have a distinct appearance, however,
       ;;; owing to the setting of `nobreak-char-display`.



(defun brec-seg-end()
  "Returns the end position of the present fontification segment, provided that point is *not*
at the beginning of the segment.  If point is at the beginning, then the result is undefined."
  (save-excursion
    (if (re-search-forward brec-seg-start-pattern nil t); Cf. `brec-extend-search-down`.
        (end-of-line 0); Moving to the end of the previous line.
      (goto-char (point-max)))
    (point)))



(defconst brec-seg-start-pattern    ; Perfect indentation (PI),          [SPC]
  "^ \\{4\\}*\\\\*[^[:space:]\\]"; zero or more backslashes (\⋯)
  ;; ┈──────┘└───┘└────────────┘ ; and a character (C) that is neither
  ;;    PI     \⋯       C        ; whitespace nor a backslash.

  "The regexp pattern of the sequence marking the start of a fontification segment
other than a document head.")



(defface brec-task-bullet-face
  `((default . (:inherit (brec-bullet-face font-lock-function-name-face))))
  "The face for the bullet of a task point.")



(defface brec-task-bullet-terminator-face
  `((default . (:inherit font-lock-comment-face)))
  "The face for the bullet terminator ‘+’ of a task point.")



;; ══════════════════════════════════════════════════════════════════════════════════════════════════════


(provide 'breccia-mode); Providing these features of `breccia-mode.el` for all who `require` them.


;; NOTES
;; ─────
;;   BUG  This is a bug.
;;
;;   D ·· Descriptor.  http://reluk.ca/project/Breccia/language_definition.brec § Descriptor
;;
;;   FLB  Font lock basics.
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Basics.html
;;
;;   FLE  Font lock extension.  The alternative to `font-lock-extend-region-functions`, namely the
;;        little used `font-lock-extend-after-change-region-function`, appears to be a design error.
;;        https://lists.gnu.org/archive/html/bug-gnu-emacs/2015-03/msg00818.html
;;
;;   OCF  Overrides in comment fontification.  The fontification of a comment must override (`t`)
;;        any fontification of its containing head, and must therefore follow it in `brec-keywords`.
;;
;;        We might have tried fontifying the commentary using the syntax system, which runs earlier.
;;        Not by mere syntax tabulation, which is unable to grasp the form of Breccian commentary;
;;        rather we would probably have used the macro `syntax-propertize-rules` to set syntax properties
;;        on the comment delimiters.  But then could the `subexp-highlighters` for the containing fractum
;;        have worked around the comments, e.g. with `override` at nil?  [SBF]
;;
;;   PSE  `pre-form` search extension: extending the end boundary of the search region
;;        for multi-line anchoring.  The manual warns, ‘It is generally a bad idea to return a position
;;        greater than the end of the line’ [SBF].  But this appears to be a bug in the manual.
;;        https://stackoverflow.com/a/9456757/2402790
;;
;;   RWC  Refontifying whitespace in comments.  It too must be refontified in order to override [OCF]
;;        any improper fontification arising from inverse labeling or user customization of faces.
;;
;;   SBF  Search-based fontification.
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
;;
;;   SF · Standard faces: `https://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html`
;;        and § Standard faces at `http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/faces.el`.
;;
;;   SPC  Synchronized pattern of commentary.  Marking an instance of a pattern or anti-pattern,
;;        one of several that together are maintained in synchrony.


                                       ;;; Copyright © 2019 Michael Allan and contributors.  Licence MIT.
