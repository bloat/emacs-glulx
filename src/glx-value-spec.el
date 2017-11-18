;;; emacs glx
;;
;; Copyright (C) 2008, 2009 Andrew Cowper
;;
;; Author: Andrew Cowper (andrew.cowper@slothrop.net)
;;
;; URL: http://github.com/bloat/emacs-glulx
;;
;; This file is licensed under the terms of the GNU General Public
;; License as distributed with Emacs (press C-h C-c to view it).

(ert-deftest test-zero ()
  "Test Zero"
  :tags '(32-bit)
  (should (equal (glx-32) glx-0))
  (should (equal (glx-+ glx-0 glx-0) glx-0))
  (should (equal (glx-+ glx-1 glx-0) glx-1)))

(ert-deftest test-not-zero ()
  "Test not zero"
  :tags '(32-bit)
  (should (equal (glx-32 34 32 54 255) (glx-32 34 32 54 255)))
  (should (equal (glx-+ (glx-32 34 32 54 255) (glx-32 10 12)) (glx-32 44 44 54 255))))

(ert-deftest test-addition-without-carry ()
  "Test addition without carry"
  :tags '(32-bit)
  (should (equal (glx-+ (glx-32 20 5 34) (glx-32 0 40 100)) (glx-32 20 45 134))))

(ert-deftest test-addition-with-carry ()
  "Test addition with carry"
  :tags '(32-bit)
  (should (equal (glx-+ (glx-32 255) (glx-32 1)) (glx-32 0 1))))

(ert-deftest test-addition-with-more-carries ()
  "Test addition with more carries"
  :tags '(32-bit)
  (should (equal (glx-+ (glx-32 255 255 167 255) (glx-32 255 255 234)) (glx-32 254 255 146))))

(ert-deftest test-addition-with-overflow ()
  "Test addition with overflow"
  :tags '(32-bit)
  (should (equal (glx-+ (glx-32 255 255 255 255) (glx-32 2)) (glx-32 1))))

(ert-deftest test-subtraction-without-carry ()
  "Test subtraction without carry"
  :tags '(32-bit)
  (should (equal (glx-- glx-4 glx-0) glx-4))
  (should (equal (glx-- glx-5 glx-3) glx-2))
  (should (equal (glx-- (glx-32 34 56 233 45) (glx-32 12 12 12 15)) (glx-32 22 44 221 30))))

(ert-deftest test-subtraction-with-carry ()
  "Test subtraction with carry"
  :tags '(32-bit)
  (should (equal (glx-- (glx-32 256) glx-1) (glx-32 255))))

(ert-deftest test-subtraction-with-more-carries ()
  "Test subtraction with more carries"
  :tags '(32-bit)
  (should (equal (glx-- (glx-32 10 102 5 14) (glx-32 15 107 209)) (glx-32 221510395))))

(ert-deftest test-subtraction-with-overflow ()
  "Test subtraction with overflow"
  :tags '(32-bit)
  (should (equal (glx-- glx-0 glx-1) (glx-32 255 255 255 255))))

(ert-deftest test-convert-to-int ()
  "Test convert to int"
  :tags '(32-bit)
  (should (equal (glx-32->int glx-0) 0))
  (should (equal (glx-32->int glx-1) 1))
  (should (equal (glx-32->int (glx-32 0 1)) 256))
  (should (equal (glx-32->int (glx-32 1 1)) 257))
  (should (equal (glx-32->int (glx-32 0 0 1)) 65536))
  (should (equal (glx-32->int (glx-32 0 0 0 1)) 16777216))
  (should (equal (glx-32->int (glx-32 255 255 255 15)) 268435455)))

(ert-deftest test-increment-32-bit ()
  "Test increment 32-bit"
  :tags '(32-bit)
  (should (equal (glx-+1 glx-0) glx-1))
  (should (equal (glx-+1 (glx-32 255)) (glx-32 0 1)))
  (should (equal (glx-+1 (glx-32 255 255 255 15)) (glx-32 0 0 0 16))))

(ert-deftest test-convert-from-int ()
  "Test convert from int"
  :tags '(32-bit)
  (should (equal (glx-32 0) glx-0))
  (should (equal (glx-32 1) glx-1))
  (should (equal (glx-32 255) (glx-32 255)))
  (should (equal (glx-32 256) (glx-32 0 1)))
  (should (equal (glx-32 268435455) (glx-32 255 255 255 15)))
  (should (equal (glx-32 -1) (glx-32 255 255 255 255)))
  (should (equal (glx-32 -3458987) (glx-- glx-0 (glx-32 3458987)))))

(ert-deftest test-truncate ()
  "Test truncate"
  :tags '(32-bit)
  (should (equal (glx-truncate (glx-32 1 2 3 4) 1) (glx-32 1)))
  (should (equal (glx-truncate (glx-32 1 2 3 4) 2) (glx-32 1 2))))

(ert-deftest test-large-value ()
  "Test large value"
  :tags '(32-bit)
  (should-error (glx-32->int (glx-32 0 0 0 #xff)) :type 'glx-value-error))

(ert-deftest test-is-negative ()
  "Test is negative"
  :tags '(32-bit)
  (should (glx-neg-p (glx-32 -1)))
  (should (glx-neg-p (glx-32 0 0 0 128)))
  (should-not (glx-neg-p glx-0))
  (should-not (glx-neg-p glx-1))
  (should-not (glx-neg-p (glx-32 255 255 255 127)))
  (should (glx-neg-p (glx-+1 (glx-32 255 255 255 127)))))

(ert-deftest test-is-positive ()
  "Test is positive"
  :tags '(32-bit)
  (should (glx-pos-p (glx-32 1)))
  (should (glx-pos-p (glx-32 255 255 255 127)))
  (should-not (glx-pos-p glx-0))
  (should-not (glx-pos-p (glx-32 -1)))
  (should-not (glx-pos-p (glx-32 0 0 0 128)))
  (should (glx-pos-p (glx-- (glx-32 0 0 0 128) glx-1))))

(ert-deftest test-bitand ()
  "Test bitand"
  :tags '(32-bit)
  (should (equal (glx-bitand (glx-32 98 44 230 104) (glx-32 23 0 255 33)) (glx-32 2 0 230 32))))

(ert-deftest test-bitor ()
  "Test bitor"
  :tags '(32-bit)
  (should (equal (glx-bitor (glx-32 98 44 230 104) (glx-32 23 0 255 33)) (glx-32 119 44 255 105))))

(ert-deftest test-bitxor ()
  "Test bitxor"
  :tags '(32-bit)
  (should (equal (glx-bitxor (glx-32 98 44 230 104) (glx-32 23 0 255 33)) (glx-32 #x75 #x2c #x19 #x49))))

(ert-deftest test-bitnot ()
  "Test bitxor"
  :tags '(32-bit)
  (should (equal (glx-bitnot (glx-32 23 0 255 33)) (glx-32 #xe8 #xff #x00 #xde))))

(ert-deftest test-shiftl ()
  "Test shift left"
  :tags '(32-bit)
  (should (equal (glx-shiftl (glx-32 23 0 255 33) (glx-32 50)) glx-0))
  (should (equal (glx-shiftl (glx-32 23 0 255 33) glx-0) (glx-32 23 0 255 33)))
  (should (equal (glx-shiftl (glx-32 23 0 255 33) glx-1) (glx-32 46 0 254 67)))
  (should (equal (glx-shiftl (glx-32 23 0 255 33) (glx-32 20)) (glx-32 24117248))))

(ert-deftest test-sshiftr ()
  "Test signed shift right"
  :tags '(32-bit)
  (should (equal (glx-sshiftr (glx-32 23 0 255 33) (glx-32 50)) glx-0))
  (should (equal (glx-sshiftr (glx-32 23 0 255 150) (glx-32 50)) (glx-32 255 255 255 255)))
  (should (equal (glx-sshiftr (glx-32 23 0 255 33) glx-1) (glx-32 285179915)))
  (should (equal (glx-sshiftr (glx-32 23 0 255 33) (glx-32 20)) (glx-32 543)))
  (should (equal (glx-sshiftr (glx-32 23 0 255 150) glx-1) (glx-32 11 128 127 203)))
  (should (equal (glx-sshiftr (glx-32 23 0 255 150) (glx-32 20)) (glx-32 111 249 255 255))))

(ert-deftest test-ushiftr ()
  "Test unsigned shift right"
  :tags '(32-bit)
  (should (equal (glx-ushiftr (glx-32 23 0 255 33) (glx-32 50)) glx-0))
  (should (equal (glx-ushiftr (glx-32 23 0 255 150) (glx-32 50)) glx-0))
  (should (equal (glx-ushiftr (glx-32 23 0 255 33) glx-1) (glx-32 285179915)))
  (should (equal (glx-ushiftr (glx-32 23 0 255 33) (glx-32 20)) (glx-32 543)))
  (should (equal (glx-ushiftr (glx-32 23 0 255 150) glx-1) (glx-32 11 128 127 75)))
  (should (equal (glx-ushiftr (glx-32 23 0 255 150) (glx-32 20)) (glx-32 2415))))

(ert-deftest test-multiplication-by-one-byte-without-carry ()
  "Test multiplication by one byte without carry"
  :tags '(32-bit)
  (should (equal (glx-*-byte (glx-32 4 5 6 7) 6) (glx-32 24 30 36 42))))

(ert-deftest test-multiplication-by-one-byte-with-carry ()
  "Test multiplication by one byte with carry"
  :tags '(32-bit)
  (should (equal (glx-*-byte (glx-32 200) 200) (glx-32 (* 200 200)))))

(ert-deftest test-multiplication-by-one-byte-with-more-carries ()
  "Test multiplication by one byte with more carries"
  :tags '(32-bit)
  (should (equal (glx-*-byte (glx-32 253 200 205 1) 10) (glx-32 226 217 9 18))))

(ert-deftest test-multiplication-with-no-carry ()
  "Test multiplication with no carry"
  :tags '(32-bit)
  (should (equal (glx-* (glx-32 4 20) (glx-32 7 8)) (glx-32 28 172 160))))

(ert-deftest test-multiplication-with-overflow ()
  "Test multiplication with overflow"
  :tags '(32-bit)
  (should (equal (glx-* (glx-32 2 3 4 5) (glx-32 7 8 9 10)) (glx-32 14 37 70 114))))

(ert-deftest test-negative-multiplication ()
  "Test negative multiplication"
  :tags '(32-bit)
  (should (equal (glx-* (glx-32 -1) (glx-32 -1)) glx-1)))

(ert-deftest test-negative-by-positive-multiplication ()
  "Test negative by positive multiplication"
  :tags '(32-bit)
  (should (equal (glx-* (glx-32 -6) (glx-32 2)) (glx-32 -12))))

(ert-deftest test-32-bit-to-signed-int ()
  "Test 32 bit to signed int"
  :tags '(32-bit)
  (should (equal (glx-s32->int (glx-32 -1)) -1)))

(ert-deftest test-large-negative-32-bit-to-signed-int ()
  "Test large negative 32 bit to signed int"
  :tags '(32-bit)
  (should (equal (glx-s32->int (glx-32 1 0 0 240)) -268435455)))

(ert-deftest test-division ()
  "Test division"
  (should (equal (glx-/ (glx-32 10) glx-2) (list (glx-32 5) glx-0)))
  (should (equal (glx-/ (glx-32 11) glx-2) (list (glx-32 5) glx-1)))
  (should (equal (glx-/ (glx-32 -11) glx-2) (list (glx-32 -5) (glx-32 -1))))
  (should (equal (glx-/ (glx-32 11) (glx-32 -2)) (list (glx-32 -5) glx-1)))
  (should (equal (glx-/ (glx-32 -11) (glx-32 -2)) (list (glx-32 5) (glx-32 -1)))))

(ert-deftest test-conversion-to-char ()
  "Test conversion to char"
  :tags '(32-bit)
  (should (equal (glx-32->char (glx-32 97)) ?a))
  (should (equal (glx-32->char (glx-32 97 5 6)) ?a)))

(ert-deftest test-conversion-to-unicode-char ()
  "Test conversion to unicode char"
  :tags '(32-bit)
  (should (equal (glx-32->unicode-char (glx-32 #x40 #x26)) ?♀)))

(ert-deftest test-truncate-hi ()
  "Test truncate - keep high bits"
  :tags '(32-bit)
  (should (equal (glx-32-hi-trunc (glx-32 9 8 7 6) 1) (glx-32 6)))
  (should (equal (glx-32-hi-trunc (glx-32 9 8 7 6) 2) (glx-32 7 6)))
  (should (equal (glx-32-hi-trunc (glx-32 9 8 7 6) 3) (glx-32 8 7 6))))

(ert-deftest test-truncate-lo ()
  "Test truncate - keep low bits"
  :tags '(32-bit)
  (should (equal (glx-32-lo-trunc (glx-32 9 8 7 6) 1) (glx-32 9)))
  (should (equal (glx-32-lo-trunc (glx-32 9 8 7 6) 2) (glx-32 9 8)))
  (should (equal (glx-32-lo-trunc (glx-32 9 8 7 6) 3) (glx-32 9 8 7))))

(ert-deftest test-unsigned-greater-than ()
  "Test unsigned greater than"
  :tags '(32-bit)
  (should (glx-32-u> (glx-32 200 200 200 200)
                     (glx-32 5 5 5 5)))
  (should (glx-32-u> (glx-32 5 5 5 200)
                     (glx-32 200 200 200 5)))
  (should (glx-32-u> (glx-32 200 200 200 200)
                     (glx-32 5 200 200 200)))
  (should-not (glx-32-u> (glx-32 200 200 200 200)
                         (glx-32 200 200 200 200))))

(ert-deftest test-unsigned-less-than ()
  "Test unsigned less than"
  :tags '(32-bit)
  (should-not (glx-32-u< (glx-32 200 200 200 200)
                         (glx-32 5 5 5 5)))
  (should-not (glx-32-u< (glx-32 5 5 5 200)
                         (glx-32 200 200 200 5)))
  (should-not (glx-32-u< (glx-32 200 200 200 200)
                         (glx-32 5 200 200 200)))
  (should-not (glx-32-u< (glx-32 200 200 200 200)
                         (glx-32 200 200 200 200))))

(ert-deftest test-to-decimal-string ()
  "Test conversion to a signed decimal string"
  :tags '(32-bit)
  (should (equal (glx-32->dec-string glx-0) "0"))
  (should (equal (glx-32->dec-string glx-2) "2"))
  (should (equal (glx-32->dec-string (glx-32 25)) "25"))
  (should (equal (glx-32->dec-string (glx-32 10 145 0 2)) "33591562"))
  (should (equal (glx-32->dec-string (glx-32 7 122 23 40)) "672627207"))
  (should (equal (glx-32->dec-string (glx-32 -1)) "-1"))
  (should (equal (glx-32->dec-string (glx-32 -25)) "-25"))
  (should (equal (glx-32->dec-string (glx-32 -33591562)) "-33591562"))
  (should (equal (glx-32->dec-string (glx-32 0 0 0 128)) "-2147483648")))
