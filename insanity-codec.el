;;; insanity-codec.el --- Encode and decode Insanity-Code-like ciphers

;; Author: B. S. Chambers <ben@bschambers.info>
;; URL: https://github.com/bschambers/elisp-insanity-codec
;; Version: 1.0.0
;;
;; Tested with: Emacs 25.3.2

(require 'cl)



;;;;;;;;;;;;;;;;;;;;;;;;;; GLOBAL VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst insanity-codec--default-cipher-alist
  '(("a" . ("97"))
    ("b" . ("98"))
    ("c" . ("99"))
    ("d" . ("100"))
    ("e" . ("101"))
    ("f" . ("102"))
    ("g" . ("103"))
    ("h" . ("104"))
    ("i" . ("105"))
    ("j" . ("106"))
    ("k" . ("107"))
    ("l" . ("108"))
    ("m" . ("109"))
    ("n" . ("110"))
    ("o" . ("111"))
    ("p" . ("112"))
    ("q" . ("113"))
    ("r" . ("114"))
    ("s" . ("115"))
    ("t" . ("116"))
    ("u" . ("117"))
    ("v" . ("118"))
    ("w" . ("119"))
    ("x" . ("120"))
    ("y" . ("121"))
    ("z" . ("122")))
  "The default cipher used - translates characters [a-zA-Z] to
  their lowercase ascii codes."  )

(defvar insanity-codec--cipher-alist '()
  "Association list defining the cipher to be used.")

(setq insanity-codec--cipher-alist insanity-codec--default-cipher-alist)

(defconst insanity-codec--magenta-ornithopter-cipher-alist
  '(("a" . ("frantic bannana"))
    ("b" . ("Theodore" "theodore"))
    ("c" . ("torque wrench"))
    ("d" . ("underscore"))
    ("e" . ("quality control supervisor"))
    ("f" . ("quality control"))
    ("g" . ("don't"))
    ("h" . ("corn"))
    ("i" . ("zoot suit"))
    ("j" . ("Bill and Ted riding the zebra bareback"))
    ("k" . ("cream cake"))
    ("l" . ("rumble strip"))
    ("m" . ("quincunx"))
    ("n" . ("dormouse"))
    ("o" . ("corncob"))
    ("p" . ("riding"))
    ("q" . ("mouse"))
    ("r" . ("and"))
    ("s" . ("country mouse"))
    ("t" . ("undermine the fortifications"))
    ("u" . ("town mouse"))
    ("v" . ("zebra"))
    ("w" . ("mortification of the flesh"))
    ("x" . ("modular"))
    ("y" . ("fortifications"))
    ("z" . ("Jeremy Corbyn")))
  "Magenta Ornithopter is a cipher devised especially for the
  testing of this package.

This cipher should not be used for any sensitive communications
since it is included in the publically available source code
distribution.

The Magenta Ornithopter cipher includes various features designed
 to test potential encoding and decoding problems. In particular,
 decoding is made more challenging by the fact that some of the
 single-word codes may also be part of the multi-word codes.")

(defvar insanity-codec-encode-retain-unknown-symbols t
  "If NON-NIL then unknown characters and square-bracket-wrapped
literal passages will be retained during encoding.

EXAMPLE:
\"Hi, Bob!\" => \"corn zoot suit ,   Theodore corncob Theodore !\"")

(defvar insanity-codec-encode-wrap-unknown-symbols t
  "If NON-NIL then any unknown characters retained during
encoding will be wrapped in square brackets.

Square-bracket-wrapped literal passages will be retained as-is,
rather than being double-wrapped.

EXAMPLE:
\"Hi, Bob!\" => \"corn zoot suit [,] [ ] Theodore corncob Theodore [!]\"")

(defvar insanity-codec-encode-unwrap-literals t
  "If NON-NIL then during encoding, any square-bracket-wrapped
literal passages will be unwrapped and then included as-it.

EXAMPLE:
\"Hi, [Bob]!\" => \"corn zoot suit ,   Bob !\"")

(defvar insanity-codec-decode-retain-unknown-symbols t
  "If NON-NIL then unknown words or symbols will be retained during decoding.

EXAMPLE:
\"corn zoot suit , blastocyst lattitude Theodore corncob Theodore !\" => \"hi,blastocystlattitudebob\"")

(defvar insanity-codec-decode-wrap-unknown-symbols t
  "If NON-NIL then any unknown words retained during decoding will be wrapped in
square brackets.


EXAMPLE:
\"corn zoot suit , blastocyst lattitude Theodore corncob Theodore !\" => \"hi[,][blastocyst][lattitude]bob[!]\"")

(defvar insanity-codec-decode-unwrap-literals t
  "If NON-NIL then during decoding, any square-bracket-wrapped
literal passages will be unwrapped and then included as-it.

EXAMPLE:
\"corn zoot suit [,] [ ] Theodore corncob Theodore [!]\" => \"hi, bob!\"")



;;;;;;;;;;;;;;;;;;;;;;;;;; GENERAL UTILITY ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insanity-codec--char-at (text index)
  (substring-no-properties text index (+ 1 index)))

(defun insanity-codec--literal-passage-at (text index)
  "Returns the square-bracketed literal passage starting at INDEX.

If the character at INDEX is not an opening square-bracket, or if
there is no matched closing square bracket then NIL is returned."
  (let ((output '())
        (num-opening-parens 0)
        (char (insanity-codec--char-at text index)))
    ;; get first char
    (if (equal "[" char)
        (progn
          (incf num-opening-parens)
          (push char output)))
    ;; if opening bracket was found, look for closing bracket
    (while (> num-opening-parens 0)
      (incf index)
      (setf char (insanity-codec--char-at text index))
      (push char output)
      (if (equal "[" char)
          (incf num-opening-parens)
        (if (equal "]" char)
            (decf num-opening-parens))))
    ;; only return output string if parens are balanced
    (if (and output
             (= 0 num-opening-parens))
        (apply 'concat (reverse output))
      nil)))

(defun insanity-codec--wrapped-literal-p (text)
  "Returns TEXT if TEXT is a properly formed bracketed literal string, otherwise returns NIL.

To be properly formed the first and last characters of the string
  must be '[' and ']' respectively."
  (if (and text
           (equal "[" (insanity-codec--char-at text 0))
           (equal "]" (insanity-codec--char-at text (- (length text) 1))))
      text
    nil))

(defun insanity-codec--unwrap-literal (text)
  (if (insanity-codec--wrapped-literal-p text)
      (substring-no-properties text 1 (- (length text) 1))
    text))

(defun insanity-codec--unpack-settings-string (text)
  "Accepts a string and returns a list of boolean values.

The length of the list returned may be anything from zero to
six.

The values of the output list are found by examining TEXT one
character at a time. 'T' or 't' count as true, 'N' or 'n' count
as false (nil). All other characters are ignored."

  (let ((index 0)
        (char nil)
        (output '()))

    (while (and index
                (< (length output) 6)
                (< index (length text)))

      ;; INDEX will be set to location of next match, or NIL ir not found
      (setf index (string-match "[tnTN]" text index))

      (if index
          (progn
            (setf char (downcase (substring text index (+ 1 index))))
            (push (equal "t" char) output)
            (incf index))))

    (reverse output)))

(defun insanity-codec--pad-and-trim-settings-list (settings)
  "If length of SETTINGS is less than six, pads out the end with
existing settings and returns the result, otherwise returns
SETTINGS unaltered.

SETTINGS must be a list of boolean values between zero and six in
length."
  (let ((output '()))
    (push (if settings (pop settings) insanity-codec-encode-retain-unknown-symbols) output)
    (push (if settings (pop settings) insanity-codec-encode-wrap-unknown-symbols) output)
    (push (if settings (pop settings) insanity-codec-encode-unwrap-literals) output)
    (push (if settings (pop settings) insanity-codec-decode-retain-unknown-symbols) output)
    (push (if settings (pop settings) insanity-codec-decode-wrap-unknown-symbols) output)
    (push (if settings (pop settings) insanity-codec-decode-unwrap-literals) output)
    (reverse output)))

(defmacro insanity-codec-do-with-settings (settings-string &rest body)
  "Temporarily assign settings and then execute body."
  `(let* ((settings (insanity-codec--pad-and-trim-settings-list
                     (insanity-codec--unpack-settings-string ,settings-string)))
          (insanity-codec-encode-retain-unknown-symbols (pop settings))
          (insanity-codec-encode-wrap-unknown-symbols (pop settings))
          (insanity-codec-encode-unwrap-literals (pop settings))
          (insanity-codec-decode-retain-unknown-symbols (pop settings))
          (insanity-codec-decode-wrap-unknown-symbols (pop settings))
          (insanity-codec-decode-unwrap-literals (pop settings)))
     ,@body))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ENCODING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insanity-codec--encode-char (char &optional retain-unknown-symbol wrap-unknown-symbol)
  "Returns the encoded value of CHAR, or NIL if CHAR is not recognised (i.e. not [a-zA-Z]).

CHAR should be a single character long string.

If RETAIN-UNKNOWN-SYMBOL is non-nil, an unknown character will be
returned as itself enclosed in square brackets - otherwise NIL
will be returned instead."
  (let ((next-word (car (cdr (assoc (downcase char) insanity-codec--cipher-alist)))))
    (if (null next-word)
        (if retain-unknown-symbol
            (if wrap-unknown-symbol
                (concat "[" char "]")
              char)
          nil)
      next-word)))

(defun insanity-codec--encode-char-at (text index &optional retain-unknown-symbol wrap-unknown-symbol)
  "Encode the char at INDEX of string TEXT."
  (insanity-codec--encode-char
   (insanity-codec--char-at text index)
   retain-unknown-symbol
   wrap-unknown-symbol))

(defun insanity-codec--encode-symbol-at
    (text index &optional retain-unknown-symbol wrap-unknown-symbol unwrap-literal)
  "Encode the symbol at INDEX of TEXT, returning a list of two elements, the encoded symbol, and the length of the symbol encoded.

The resulting encoded symbol may be either a character, NIL, or a
square-bracketed literal passage.

A literal passage will only be returned if INDEX falls on the
opening square-bracket AND there is a matching closing
square-bracket later in the string."
  (let ((char (insanity-codec--char-at text index))
        ;; will be NIL unless a well-formed literal passage begins at index
        (literal (insanity-codec--literal-passage-at text index)))

    ;; literal passage
    (if literal
        (list
         (if (not retain-unknown-symbol)
             nil
           (if unwrap-literal
               (insanity-codec--unwrap-literal literal)
             literal))
         (length literal))

      ;; any other (single character) symbol
      (list
       (insanity-codec--encode-char char retain-unknown-symbol wrap-unknown-symbol)
       1))))

(defun insanity-codec--do-encode-string (text)
  "Encode TEXT using Odin's Insanity Code."
  (interactive "sEnter a string: ")
  ;; Iterate TEXT translating each character & accumulating result in WORD-LIST.
  (let ((index 0)
        (word-list '()))
    (while (< index (length text))

      (destructuring-bind
          (next-symbol
           step)
          (insanity-codec--encode-symbol-at
           text
           index
           insanity-codec-encode-retain-unknown-symbols
           insanity-codec-encode-wrap-unknown-symbols
           insanity-codec-encode-unwrap-literals)

        (if next-symbol
            (progn
              (if word-list (push " " word-list)) ; add space if NOT the first word
              (push next-symbol word-list)))

        (incf index step)))

    ;; concatenate WORD-LIST into a single string
    (apply 'concat (reverse word-list))))

(defun insanity-codec-encode-string (text &optional settings-string)
  "Encode TEXT using Odin's Insanity Code, optionally applying
temporary settings during operation."
  (if settings-string
      (insanity-codec-do-with-settings settings-string (insanity-codec--do-encode-string text))
    (insanity-codec--do-encode-string text)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DECODING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insanity-codec--split-string (text)
  "Split string by whitespace, but treat any passage contained
within square brackets as a single word, even if it contains
whitespace."
  (let ((index 0)
        (start 0)
        (num-parens-open 0)
        (word-list '())
        (current-char ""))
    ;; iterate text one character at a time
    (while (< index (length text))
      (setf current-char (substring text index (+ 1 index)))
      ;; count number of square brackets opened & closed
      (if (equalp "[" current-char)
          (incf num-parens-open)
        (if (equalp "]" current-char)
            (decf num-parens-open)
          ;; if square brackets are balanced, check for space
          (if (and (= 0 num-parens-open)
                   (equalp current-char " "))
              (progn
                ;; only add string if has non-zero length
                (if (not (= start index))
                    (push (substring text start index) word-list))
                (setf start (+ 1 index))))))
      (incf index))
    ;; add final word
    (if (not (= start index))
        (push (substring text start index) word-list))
    (reverse word-list)))

(defun insanity-codec--concat-word-spaced (&rest snippets)
  "Concatenates text snippets placing a single space between
 them, and making sure that there is no preceeding or trailing
 space.  NOTE: does not check for incorrect spacing within
 multi-word snippets, only trims front and back."
  (let ((output-list '()))
    (dolist (snip snippets)
      (if (not (string-empty-p snip))
          (progn
            (if output-list (push " " output-list))
            (push (string-trim snip) output-list))))
    (apply 'concat (reverse output-list))))

(defun insanity-codec--push-if (item place)
  "Push ITEM to PLACE if it is not nil"
  (if item (push item place))
  place)

(defun insanity-codec--decode-get-matches (text)
  "Gets a list of alist entries for which TEXT matches the beginning"
  (let ((matches '()))
    (dolist (item insanity-codec--cipher-alist)
      (let ((matched nil))
        ;; some items have multiple item-codes in cdr, so must check each one
        (dolist (item-code (cdr item))
          (if (string-prefix-p text item-code)
              (setf matched t)))
        (if matched (push item matches))))
    matches))

(defun insanity-codec--get-chunk-if-complete (text matches)
  "Compares TEXT against list of MATCHES. Returns the first
  complete match, or NIL if there is no complete match.

MATCHES is a list of code-alist entries."
  (let ((output nil)
        (current-string ""))
    (dolist (current-match matches)
      ;; some items have multiple item-codes in cdr, so must check each one
      (dolist (current-string (cdr current-match))
        (if (string-equal text current-string)
            (setq output current-match))))
    output))

(defun insanity-codec--do-decode-string (text)
  "Convert an Insanity Code encoded string to plain text."
  (interactive "sEnter an Insanity Code encoded string: ")

  ;; DECODING METHOD:
  ;; split TEXT into WORDS-LIST
  ;; iterate words in WORD-LIST
  ;; | add next word to CURRENT-CHUNK
  ;; | get list of partial matches for CURRENT-CHUNK
  ;; | is chunk valid?
  ;; | | yes:
  ;; | |   is chunk complete?
  ;; | |     yes:
  ;; | |       push to front of complete-chunks list
  ;; | | no:
  ;; |   | are there any complete chunks?
  ;; |   | | yes:
  ;; |   | |   add current longest complete chunk to output list...
  ;; |   | |   ... & subtract it from the beginning of current chunk
  ;; |   | |   push remaining parts of current-chunk back on to WORDS-LIST
  ;; |   | | no:
  ;; |   |     discard and add query string to output
  ;; |   |
  ;; |   | clear the complete-chunks list
  ;; |   | set current-chunk to empty string
  ;;
  ;; do any complete-chunks remain?
  ;; | yes:
  ;; |   add to OUTPUT-LIST
  ;;
  ;; make OUTPUT-LIST into string and return it

  (let ((words-list (insanity-codec--split-string text))
        (current-chunk "")
        (matches '())
        (is-valid nil)
        (complete-chunks '())
        (output-list '()))

    (while words-list
      ;; assign variables
      (setq current-chunk (insanity-codec--concat-word-spaced current-chunk (pop words-list)))
      (setq matches (insanity-codec--decode-get-matches current-chunk))
      (setq is-valid (> (length matches) 0))

      (if is-valid
          ;; CHUNK IS VALID: check whether it is complete
          (setf complete-chunks
                (insanity-codec--push-if
                 (insanity-codec--get-chunk-if-complete current-chunk matches)
                 complete-chunks))

        ;; CHUNK NOT VALID:
        (progn
          (if complete-chunks
              ;; get most recent complete chunk
              ;; and separate off the invalid section from the end of the current chunk
              (let* ((recent-chunk (pop complete-chunks))
                     (remaining (substring current-chunk (length (cadr recent-chunk)))))
                (push (car recent-chunk) output-list)
                ;; add the remaining parts back on to WORDS-LIST
                (dolist (part (insanity-codec--split-string remaining))
                  (push part words-list)))

            ;; no complete chunks: retain untranslated chunk if required by settings
            (if insanity-codec-decode-retain-unknown-symbols
                ;; WRAPPED LITERAL: unwrap bracketed literal if required by settings
                (if (insanity-codec--wrapped-literal-p current-chunk)
                    (if insanity-codec-decode-unwrap-literals
                        (push (insanity-codec--unwrap-literal current-chunk) output-list)
                      (push current-chunk output-list))
                  ;; OTHER UNKNOWN SYMBOL: add wrapping if required by settings
                  (if insanity-codec-decode-wrap-unknown-symbols
                      (push (concat "[" current-chunk "]") output-list)
                    (push current-chunk output-list)))))

          ;; always reset CURRENT-CHUNK and COMPLETE-CHUNKS list
          (setq current-chunk ""
                complete-chunks '()))))

    ;; if any complete chunk remains then add to the output list
    (if complete-chunks
        (dolist (new-chunk complete-chunks)
          (push (car new-chunk) output-list)))

    ;; convert output-list to string
    (apply 'concat (reverse output-list))))

(defun insanity-codec-decode-string (text &optional settings-string)
  "Convert an Insanity Code encoded string to plain text,
optionally applying temporary settings during operation."
  (if settings-string
      (insanity-codec-do-with-settings settings-string (insanity-codec--do-decode-string text))
    (insanity-codec--do-decode-string text)))



;;;;;;;;;;;;;;;; SHORTCUT FUNCTIONS: eval if desired ;;;;;;;;;;;;;;;;;

;; (defun encode (text &optional settings) (insanity-codec-encode-string text settings))
;; (defun decode (text &optional settings) (insanity-codec-decode-string text settings))
;; (defun get-matches (text) (insanity-codec--decode-get-matches text))

;; Eval these to use either the Magenta Ornithopter cipher or the default cipher...
;; ... note: to use Magenta Ornithopter, eval or load-file tests.el first...
;; (setf insanity-codec--cipher-alist insanity-codec--magenta-ornithopter-cipher-alist)
;; (setf insanity-codec--cipher-alist insanity-codec--default-cipher-alist)



(provide 'insanity-codec)
