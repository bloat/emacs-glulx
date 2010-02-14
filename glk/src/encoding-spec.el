(context "Character encoding: "
         (tag encoding)

         (specify "Should be able to convert to lower case"
                  (expect (glk-char-to-lower ?A) equals ?a)
                  (expect (glk-char-to-lower ?C) equals ?c)
                  (expect (glk-char-to-lower ?b) equals ?b)
                  (expect (glk-char-to-lower ?e) equals ?e)))


