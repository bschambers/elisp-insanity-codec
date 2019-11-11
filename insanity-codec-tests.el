;;; insanity-codec-tests.el --- Unit tests for insanity-codec.el

;; To run tests, eval this file then M-x ert
;;
;; Author: B. S. Chambers <ben@bschambers.info>
;; URL: https://github.com/bschambers/elisp-insanity-codec
;; Version: 1.0.0
;;
;; Tested with: Emacs 25.3.2

;; Local variables:
;; nameless-aliases: (("ic" . "insanity-codec"))
;; End:

(require 'insanity-codec)

;;;;;;;;;;;;;;;;;;;;; GENERAL UTILITY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest insanity-codec-tests-get-char-at-index-of-string ()
  (should (equal "a" (insanity-codec--char-at "abcdefg" 0)))
  (should (equal "c" (insanity-codec--char-at "abcdefg" 2)))
  (should (equal "g" (insanity-codec--char-at "abcdefg" 6))))

(ert-deftest insanity-codec-tests-get-literal-passage-at-index-of-string ()
  (let ((text "abc [granny [smith]] cardboard [box]"))
    (should (equal nil (insanity-codec--literal-passage-at text 10)))
    (should (equal "[smith]" (insanity-codec--literal-passage-at text 12)))
    (should (equal "[granny [smith]]" (insanity-codec--literal-passage-at text 4)))))

(ert-deftest insanity-codec-tests-recognise-wrapped-literal ()
  (should (insanity-codec--wrapped-literal-p "[plop]"))
  (should (insanity-codec--wrapped-literal-p "[!]"))
  (should (insanity-codec--wrapped-literal-p "[ ]"))
  (should (insanity-codec--wrapped-literal-p "[ blah blah pingu!]"))
  (should-not (insanity-codec--wrapped-literal-p "plop"))
  (should-not (insanity-codec--wrapped-literal-p "!"))
  (should-not (insanity-codec--wrapped-literal-p " "))
  ;; fail if not properly formed
  (should-not (insanity-codec--wrapped-literal-p "[a"))
  (should-not (insanity-codec--wrapped-literal-p "[a] "))
  (should-not (insanity-codec--wrapped-literal-p " [a]")))

(ert-deftest insanity-codec-tests-unwrap-wrapped-literal ()
  (should (equalp "abc" (insanity-codec--unwrap-literal "[abc]")))
  (should (equalp "abc 123 xyz" (insanity-codec--unwrap-literal "[abc 123 xyz]")))
  (should (equalp " " (insanity-codec--unwrap-literal "[ ]")))
  ;; don't unwrap if not a properly formed wrapped literal
  (should (equalp "floppy" (insanity-codec--unwrap-literal "floppy")))
  (should (equalp "    floppy " (insanity-codec--unwrap-literal "    floppy ")))
  (should (equalp "[abc" (insanity-codec--unwrap-literal "[abc")))
  (should (equalp " [abc]" (insanity-codec--unwrap-literal " [abc]"))))

(ert-deftest insanity-codec-tests-concatenate-words-with-single-space ()
  (should (equalp "abc 123" (insanity-codec--concat-word-spaced "abc" "123")))
  (should (equalp "abc 123 xyz" (insanity-codec--concat-word-spaced "abc" "     123 " " xyz   ")))
  (should (equalp "abc [123]" (insanity-codec--concat-word-spaced "abc" "[123]")))
  (should (equalp "abc [   123   ]" (insanity-codec--concat-word-spaced "abc" "[   123   ]")))
  (should (equalp "nicety [ ] duck [ ] vendor" (insanity-codec--concat-word-spaced "nicety" "[ ]" "duck" "[ ]" "vendor"))))

(ert-deftest insanity-codec-tests-unpack-settings-string ()
  (should (equal '(nil nil) (insanity-codec--unpack-settings-string "nN")))
  (should (equal '(t t) (insanity-codec--unpack-settings-string "tT")))
  (should (equal '(t nil t nil t t) (insanity-codec--unpack-settings-string "tnTNtTtntntNNN")))
  (should (equal '(t t nil t t t)
                 (insanity-codec--unpack-settings-string
                  "Troglodytes frantically sacked the great Tree Fort of Weston-super-Mare!")))
  (should (equal '(nil nil nil nil nil nil)
                 (insanity-codec--unpack-settings-string
                  "Beginning under John's sub-basement, the fissure extends deep into the centre of the earth."))))

(ert-deftest insanity-codec-tests-pad-and-trim-settings-list ()
  (let ((insanity-codec-encode-retain-unknown-symbols t)
        (insanity-codec-encode-wrap-unknown-symbols nil)
        (insanity-codec-encode-unwrap-literals t)
        (insanity-codec-decode-retain-unknown-symbols nil)
        (insanity-codec-decode-wrap-unknown-symbols nil)
        (insanity-codec-decode-unwrap-literals t))
    (should (equal '(t nil t nil nil t) (insanity-codec--pad-and-trim-settings-list '())))
    (should (equal '(t t t nil nil t) (insanity-codec--pad-and-trim-settings-list '(t t))))
    (should (equal '(t t nil nil t nil) (insanity-codec--pad-and-trim-settings-list '(t t nil nil t nil))))
    (should (equal '(nil nil nil t t t) (insanity-codec--pad-and-trim-settings-list '(nil nil nil t t t nil t t t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ENCODING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest insanity-codec-tests-valid-chars-are-encoded ()
  :tags '(the-insanity-code)
  (let ((insanity-codec--cipher-alist insanity-codec--magenta-ornithopter-cipher-alist))
    (should (equal "don't" (insanity-codec--encode-char "g")))
    (should (equal "corn" (insanity-codec--encode-char "h")))
    (should (equal "Bill and Ted riding the zebra bareback" (insanity-codec--encode-char "j")))
    (should (equal "mortification of the flesh" (insanity-codec--encode-char "w")))
    ;; uppercase work just the same
    (should (equal "don't" (insanity-codec--encode-char "G")))
    (should (equal "corn" (insanity-codec--encode-char "H")))))

(ert-deftest insanity-codec-tests-unknown-chars-discarded ()
  (should (equal nil (insanity-codec--encode-char "&")))
  (should (equal nil (insanity-codec--encode-char "#")))
  (should (equal nil (insanity-codec--encode-char "?"))))

(ert-deftest insanity-codec-tests-unknown-chars-retained-unwrapped ()
  (should (equal "&" (insanity-codec--encode-char "&" t)))
  (should (equal "#" (insanity-codec--encode-char "#" t)))
  (should (equal "?" (insanity-codec--encode-char "?" t nil))))

(ert-deftest insanity-codec-tests-unknown-chars-retained-wrapped ()
  (should (equal "[&]" (insanity-codec--encode-char "&" t t)))
  (should (equal "[#]" (insanity-codec--encode-char "#" t t)))
  (should (equal "[?]" (insanity-codec--encode-char "?" t t))))

(ert-deftest insanity-codec-tests-digits-are-unknown-chars ()
  (should (equal nil (insanity-codec--encode-char "3")))
  (should (equal nil (insanity-codec--encode-char "0")))
  (should (equal "7" (insanity-codec--encode-char "7" t)))
  (should (equal "1" (insanity-codec--encode-char "1" t)))
  (should (equal "[8]" (insanity-codec--encode-char "8" t t)))
  (should (equal "[2]" (insanity-codec--encode-char "2" t t))))

(ert-deftest insanity-codec-tests-handle-literal-passages-while-encoding ()
  (let ((insanity-codec--cipher-alist insanity-codec--magenta-ornithopter-cipher-alist)
        (text "abc * 123 [granny [smith]] cardboard [box]"))
    (should (equal '("frantic bannana" 1) (insanity-codec--encode-symbol-at text 0)))
    (should (equal '("don't" 1) (insanity-codec--encode-symbol-at text 11)))
    (should (equal '(nil 1) (insanity-codec--encode-symbol-at text 3)))
    (should (equal '(" " 1) (insanity-codec--encode-symbol-at text 3 t)))
    (should (equal '("[ ]" 1) (insanity-codec--encode-symbol-at text 3 t t)))
    (should (equal '("[granny [smith]]" 16) (insanity-codec--encode-symbol-at text 10 t)))
    (should (equal '("[smith]" 7) (insanity-codec--encode-symbol-at text 18 t)))
    ;; literal passages should not be wrapped again, even when WRAP-UNKNOWN-SYMBOLS = t
    (should (equal '("[granny [smith]]" 16) (insanity-codec--encode-symbol-at text 10 t t)))
    (should (equal '("[smith]" 7) (insanity-codec--encode-symbol-at text 18 t t)))
    ;; literal passage uwrapped if settings require it
    (should (equal '("granny [smith]" 16) (insanity-codec--encode-symbol-at text 10 t t t)))))

(ert-deftest insanity-codec-tests-encode-string-discarding-unknown-symbols ()
  :tags '(the-insanity-code)
  (let ((insanity-codec-encode-retain-unknown-symbols nil)
        (insanity-codec--cipher-alist insanity-codec--magenta-ornithopter-cipher-alist))
    (should (equal "zoot suit frantic bannana quincunx Theodore zoot suit rumble strip rumble strip"
                   (insanity-codec-encode-string "I am Bill!")))
    (should (equal "country mouse undermine the fortifications corncob quality control quincunx frantic bannana fortifications"
                   (insanity-codec-encode-string "21st of May")))))

(ert-deftest insanity-codec-tests-encode-string-retaining-unknown-symbols-wrapped ()
  :tags '(the-insanity-code)
  (let ((insanity-codec-encode-retain-unknown-symbols t)
        (insanity-codec-encode-wrap-unknown-symbols t)
        (insanity-codec-encode-unwrap-literals nil)
        (insanity-codec--cipher-alist insanity-codec--magenta-ornithopter-cipher-alist))
    (should (equal "corn quality control supervisor rumble strip rumble strip corncob [!]"
                   (insanity-codec-encode-string "Hello!")))
    (should (equal "zoot suit [ ] frantic bannana quincunx [ ] Theodore zoot suit rumble strip rumble strip [!]"
                   (insanity-codec-encode-string "I am Bill!")))
    (should (equal "[2] [1] country mouse undermine the fortifications [ ] corncob quality control [ ] quincunx frantic bannana fortifications"
                   (insanity-codec-encode-string "21st of May")))
    ;; don't encode square-bracketed literal passages
    (should (equal "[2] [1] country mouse undermine the fortifications [ ] corncob quality control [fungible hacienda]"
                   (insanity-codec-encode-string "21st of[fungible hacienda]")))))

(ert-deftest insanity-codec-tests-encode-string-retaining-unknown-symbols-unwrapped ()
  :tags '(the-insanity-code)
  (let ((insanity-codec-encode-retain-unknown-symbols t)
        (insanity-codec-encode-wrap-unknown-symbols nil)
        (insanity-codec-encode-unwrap-literals nil)
        (insanity-codec--cipher-alist insanity-codec--magenta-ornithopter-cipher-alist))
    (should (equal "corn quality control supervisor rumble strip rumble strip corncob !"
                   (insanity-codec-encode-string "hello!")))
    (should (equal "zoot suit   frantic bannana quincunx   Theodore zoot suit rumble strip rumble strip !"
                   (insanity-codec-encode-string "I am Bill!")))
    (should (equal "2 1 country mouse undermine the fortifications   corncob quality control   quincunx frantic bannana fortifications"
                   (insanity-codec-encode-string "21st of May")))
    ;; don't encode square-bracketed literal passages
    (should (equal "2 1 country mouse undermine the fortifications   corncob quality control [fungible hacienda]"
                   (insanity-codec-encode-string "21st of[fungible hacienda]")))))

(ert-deftest insanity-codec-tests-encode-string-unwrapping-literals ()
  :tags '(the-insanity-code)
  (let ((insanity-codec-encode-retain-unknown-symbols t)
        (insanity-codec-encode-wrap-unknown-symbols t)
        (insanity-codec-encode-unwrap-literals t)
        (insanity-codec--cipher-alist insanity-codec--magenta-ornithopter-cipher-alist))
    (should (equal "[2] [1] country mouse undermine the fortifications [ ] corncob quality control fungible hacienda"
                   (insanity-codec-encode-string "21st of[fungible hacienda]")))))

(ert-deftest insanity-codec-tests-encode-with-settings-string ()
  (let ((text "I [Gordon] am Bill!")
        (insanity-codec--cipher-alist insanity-codec--magenta-ornithopter-cipher-alist))
    (should (equal
             "zoot suit [ ] [Gordon] [ ] frantic bannana quincunx [ ] Theodore zoot suit rumble strip rumble strip [!]"
             (insanity-codec-encode-string text "ttn")))
    (should (equal
             "zoot suit   [Gordon]   frantic bannana quincunx   Theodore zoot suit rumble strip rumble strip !"
             (insanity-codec-encode-string text "tnnttt")))
    (should (equal
             "zoot suit frantic bannana quincunx Theodore zoot suit rumble strip rumble strip"
             (insanity-codec-encode-string text "nttntt")))
    (should (equal
             "zoot suit [ ] Gordon [ ] frantic bannana quincunx [ ] Theodore zoot suit rumble strip rumble strip [!]"
             (insanity-codec-encode-string text "ttt")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DECODING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest insanity-codec-tests-during-split-square-bracketed-text-treated-as-single-word ()
  (should (equalp '("hang" "glider" "library" "library" "liability")
                  (insanity-codec--split-string "hang glider library library liability")))
  (should (equalp '("nicety" "[ ]" "duck" "vendor" "[ ]" "sandwich" "train" "liability")
                  (insanity-codec--split-string "nicety [ ] duck vendor [ ] sandwich train liability")))
  (should (equalp '("dog" "[bannana flap jack ]" "apple" "[....!]" "crown" "portability")
                  (insanity-codec--split-string "dog [bannana flap jack ] apple [....!] crown portability")))
  ;; handles nested square brackets also
  (should (equalp '("dog" "[bannana [flap] jack ]" "apple" "[[crown]]")
                  (insanity-codec--split-string "dog [bannana [flap] jack ] apple [[crown]]"))))

(ert-deftest insanity-codec-tests-during-split-multiple-spaces-collapsed ()
  (should (equalp '("hang" "glider" "library" "liability")
                  (insanity-codec--split-string "hang          glider library    liability")))
  (should (equalp '("nicety" "[ ]" "duck" "vendor")
                  (insanity-codec--split-string "          nicety [ ]  duck vendor            "))))

(ert-deftest insanity-codec-tests-join-strings-with-no-preceding-or-trailing-whitespace ()
  (should (equal "bill" (insanity-codec--concat-word-spaced " bill ")))
  (should (equal "bill" (insanity-codec--concat-word-spaced "" " bill ")))
  (should (equal "hi bill" (insanity-codec--concat-word-spaced "   hi " " bill ")))
  (should (equal "tie me kangaroo down sport"
                 (insanity-codec--concat-word-spaced " tie " "me " "kangaroo" " down     " "sport    "))))

(ert-deftest insanity-codec-tests-push-item-if-not-nil ()
  (let ((words '("bongo" "polo")))
    (should (equalp '("bongo" "polo") (insanity-codec--push-if nil words)))
    (should (equalp '("blah" "bongo" "polo") (insanity-codec--push-if "blah" words)))))

(ert-deftest insanity-codec-tests-get-matches-as-list-of-alist-entries ()
  (let ((insanity-codec--cipher-alist insanity-codec--magenta-ornithopter-cipher-alist))
    (let ((matches (insanity-codec--decode-get-matches "t")))
      (should (eq 3 (length matches)))
      (should (member '("b" "Theodore" "theodore")  matches))
      (should (member '("c" "torque wrench")  matches))
      (should (member '("u" "town mouse")  matches)))))

(ert-deftest insanity-codec-tests-get-matches-even-when-matching-cdr-of-match ()
  (let ((insanity-codec--cipher-alist insanity-codec--magenta-ornithopter-cipher-alist))
    (let ((matches (insanity-codec--decode-get-matches "theodore")))
      (should (equalp matches '(("b" "Theodore" "theodore")))))))

(ert-deftest insanity-codec-tests-recognises-complete-chunk-if-in-cdr-of-match ()
  (should (equal '("q" "blah")
                 (insanity-codec--get-chunk-if-complete "blah" '(("n" "bill")
                                                          ("q" "blah")))))
  (should (equal nil
                 (insanity-codec--get-chunk-if-complete "flop" '(("n" "bill")
                                                          ("q" "blah"))))))

(ert-deftest insanity-codec-tests-recognises-complete-chunk-if-in-rear-cdr-of-match ()
  (should (equal '("q" "blimp" "bozo" "blah")
                 (insanity-codec--get-chunk-if-complete "blah" '(("n" "bill")
                                                          ("q" "blimp" "bozo" "blah")))))
  (should (equal '("b" "zipper" "plonker" "blah" "turbulence")
                 (insanity-codec--get-chunk-if-complete "plonker"
                                                 '(("a" "zipper" "turbulence" "bill" "plop")
                                                   ("b" "zipper" "plonker" "blah" "turbulence")
                                                   ("c" "zipper" "plonker-time" "blah" "turbulence"))))))

(ert-deftest insanity-codec-tests-ignores-complete-chunk-if-in-car-of-match ()
  (should (null
           (insanity-codec--get-chunk-if-complete "blah" '(("n" "bill")
                                                    ("blah" "balls" "blip"))))))

(ert-deftest insanity-codec-tests-decode-string-discarding-unknown-words ()
  (let ((insanity-codec-decode-retain-unknown-symbols nil)
        (insanity-codec-decode-unwrap-literals nil)
        (insanity-codec--cipher-alist insanity-codec--magenta-ornithopter-cipher-alist))
    (should (equal "hi"
                   (insanity-codec-decode-string "corn faberge zoot suit !")))
    (should (equal "abc"
                   (insanity-codec-decode-string "frantic bannana literal Theodore [ ] torque wrench mint julep")))))

(ert-deftest insanity-codec-tests-decode-string-retaining-unknown-words-wrapped ()
  (let ((insanity-codec-decode-retain-unknown-symbols t)
        (insanity-codec-decode-wrap-unknown-symbols t)
        (insanity-codec-decode-unwrap-literals nil)
        (insanity-codec--cipher-alist insanity-codec--magenta-ornithopter-cipher-alist))
    (should (equal "[coming]soon"
                   (insanity-codec-decode-string "coming country mouse corncob corncob dormouse")))
    (should (equal "h[fart][face]ello[ping][pong]"
                   (insanity-codec-decode-string "corn fart face quality control supervisor rumble strip rumble strip corncob ping pong   ")))))

(ert-deftest insanity-codec-tests-decode-string-retaining-unknown-words-unwrapped ()
  (let ((insanity-codec-decode-retain-unknown-symbols t)
        (insanity-codec-decode-wrap-unknown-symbols nil)
        (insanity-codec-decode-unwrap-literals nil)
        (insanity-codec--cipher-alist insanity-codec--magenta-ornithopter-cipher-alist))
    (should (equal "hello!"
                   (insanity-codec-decode-string "corn quality control supervisor rumble strip rumble strip corncob !")))
    (should (equal "comingsoon"
                   (insanity-codec-decode-string "coming country mouse corncob corncob dormouse")))
    (should (equal "hfartfaceellopingpong"
                   (insanity-codec-decode-string "corn fart face quality control supervisor rumble strip rumble strip corncob ping pong   ")))))

(ert-deftest insanity-codec-tests-decode-string-unwrapping-literals ()
  (let ((insanity-codec-decode-retain-unknown-symbols t)
        (insanity-codec-decode-unwrap-literals t)
        (insanity-codec--cipher-alist insanity-codec--magenta-ornithopter-cipher-alist))
    (should (equalp "hi!"
                    (insanity-codec-decode-string "corn zoot suit [!]")))
    (should (equalp "I am Ben"
                    (insanity-codec-decode-string "zoot suit [ ] frantic bannana quincunx [ ] theodore quality control supervisor dormouse")))))

(ert-deftest insanity-codec-tests-decode-multi-word-character-codes-even-when-they-contain-other-codes ()
  "j = \"don't undermine the cream cake quality control\"...
... this contains \"don't\" (g), and \"quality control\" (f)."
  (let ((insanity-codec-decode-retain-unknown-symbols nil)
        (insanity-codec-decode-wrap-unknown-symbols nil)
        (insanity-codec-decode-unwrap-literals nil)
        (insanity-codec--cipher-alist insanity-codec--magenta-ornithopter-cipher-alist))
    (should (equal "fishjam"
                   (insanity-codec-decode-string
                    "quality control zoot suit country mouse corn Bill and Ted riding the zebra bareback frantic bannana quincunx")))))

(ert-deftest insanity-codec-tests-decode-with-settings-string ()
  (let ((insanity-codec--cipher-alist insanity-codec--magenta-ornithopter-cipher-alist)
        (text "zoot suit [ ] [Gordon] flipper [ ] frantic bannana quincunx [ ] Theodore zoot suit rumble strip rumble strip [!]"))
    ;; discard unknown
    (should (equal
             "iambill"
             (insanity-codec-decode-string text "tttntt")))
    ;; keep unknown
    (should (equal
             "i[ ][Gordon]flipper[ ]am[ ]bill[!]"
             (insanity-codec-decode-string text "tnntnn")))
    ;; retain and wrap unknown
    (should (equal
             "i[ ][Gordon][flipper][ ]am[ ]bill[!]"
             (insanity-codec-decode-string text "nttttn")))
    ;; retain and wrap unknown, unwrap literals
    (should (equal
             "i Gordon[flipper] am bill!"
             (insanity-codec-decode-string text "tttttt")))
    ;; retain unknown, unwrap literals
    (should (equal
             "i Gordonflipper am bill!"
             (insanity-codec-decode-string text "ttttnt")))))
