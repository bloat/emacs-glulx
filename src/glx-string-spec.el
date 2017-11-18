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

(ert-deftest decode-a-zero-byte-uncompressed-string ()
  "Decode a zero byte uncompressed string"
  :tags '(string)
  (let ((*glx-memory* [#xe0 0]))
    (should (equal (glx-get-string glx-0) ""))))

(ert-deftest decode-a-two-byte-uncompressed-string ()
  "Decode a two byte uncompressed string"
  :tags '(string)
  (let ((*glx-memory* [#xe0 65 66 0]))
    (should (equal (glx-get-string glx-0) "AB"))))

(ert-deftest get-the-root-node-for-a-string-table ()
  "Get the root node for a string table"
  :tags '(string)
  (let ((*glx-memory* [nil 0 0 0 13 0 0 0 1 0 0 0 14 1])
        (*glx-string-table* glx-1))
    (should (equal (glx-st-get-root-node-ptr) (glx-32 14)))))

(ert-deftest get-a-bitstream-from-a-memory-location2 ()
  "Get a bitstream from a memory location2"
  :tags '(string)
  (let ((*glx-memory* [#b10101110 #b10101101]))
    (let ((glx-bitstream (glx-get-bitstream glx-0)))
      (should-not (glx-next-bit glx-bitstream))
      (should (glx-next-bit glx-bitstream))
      (should (glx-next-bit glx-bitstream))
      (should (glx-next-bit glx-bitstream))
      (should-not (glx-next-bit glx-bitstream))
      (should (glx-next-bit glx-bitstream))
      (should-not (glx-next-bit glx-bitstream))
      (should (glx-next-bit glx-bitstream))
      (should (glx-next-bit glx-bitstream))
      (should-not (glx-next-bit glx-bitstream)))))

(ert-deftest use-terminator-node-for-compressed-string ()
  "Use terminator node for compressed string"
  :tags '(string)
  (let ((*glx-memory* [nil 0 0 0 13 0 0 0 1 0 0 0 13 1 #xe1])
        (*glx-string-table* glx-1))
    (should (equal (glx-get-string (glx-32 14)) ""))))

(ert-deftest branch-on-branch-node ()
  "Branch on branch node"
  :tags '(string)
  (let ((*glx-memory* [nil 0 0 0 23 0 0 0 3 0 0 0 13 0 0 0 0 22 0 0 0 23 1 1 #xe1 0 #xe1 1])
        (*glx-string-table* glx-1))
    (should (equal (glx-get-string (glx-32 24)) ""))
    (should (equal (glx-get-string '(0 0 0 26)) ""))))

(ert-deftest single-character-leaf-node ()
  "Single character leaf node"
  :tags '(string)
  (let ((*glx-memory* [nil 0 0 0 24 0 0 0 3 0 0 0 13 0 0 0 0 22 0 0 0 24 2 ?A 1 #xe1 2 #xe1 1])
        (*glx-string-table* glx-1))
    (should (equal (glx-get-string (glx-32 25)) "A"))
    (should (equal (glx-get-string (glx-32 27)) ""))))

(ert-deftest string-leaf-node ()
  "String leaf node"
  :tags '(string)
  (let ((*glx-memory* [nil 0 0 0 24 0 0 0 3 0 0 0 13 0 0 0 0 22 0 0 0 27 3 ?A ?B ?C 0 1 #xe1 2 #xe1 1])
        (*glx-string-table* glx-1))
    (should (equal (glx-get-string (glx-32 28)) "ABC"))
    (should (equal (glx-get-string (glx-32 30)) ""))))

(ert-deftest string-and-single-character-leaf-nodes ()
  "String and single character leaf nodes"
  :tags '(string)
  (let ((*glx-memory*
         [nil
          0 0 0 49                    ;4
          0 0 0 7                     ;8
          0 0 0 13                    ;12
          0 0 0 0 22 0 0 0 31         ;21
          0 0 0 0 40 0 0 0 45         ;30
          0 0 0 0 46 0 0 0 48         ;39
          3 ?A ?B ?C 0                ;44
          1                           ;45
          2 ?J                        ;47
          2 ?P                        ;49
          #xe1 #b00100000
          #xe1 #b10010011])
        (*glx-string-table* glx-1))
    (should (equal (glx-get-string (glx-32 50)) "ABCABC"))
    (should (equal (glx-get-string (glx-32 52)) "PABCJ"))))

(ert-deftest test-indirect-string-reference ()
  "Test indirect string reference"
  :tags '(string)
  (let ((*glx-memory*
         [nil
          0 0 0 52                    ;4
          0 0 0 7                     ;8
          0 0 0 13                    ;12
          0 0 0 0 22 0 0 0 31         ;21
          0 0 0 0 40 0 0 0 45         ;30
          0 0 0 0 46 0 0 0 51         ;39
          3 ?A ?B ?C 0                ;44
          1                           ;45
          8 0 0 0 55                  ;50
          2 ?P                        ;52
          #xe1 #b10000100
          #xe1 #b00001011])
        (*glx-string-table* glx-1))
    (should (equal (glx-get-string (glx-32 55)) "P"))
    (should (equal (glx-get-string (glx-32 53)) "ABCPABC"))))

(ert-deftest test-double-indirect-string-reference ()
  "Test double indirect string reference"
  :tags '(string)
  (let ((*glx-memory*
         [nil
          0 0 0 52                    ;4
          0 0 0 7                     ;8
          0 0 0 13                    ;12
          0 0 0 0 22 0 0 0 31         ;21
          0 0 0 0 40 0 0 0 45         ;30
          0 0 0 0 46 0 0 0 51         ;39
          3 ?A ?B ?C 0                ;44
          1                           ;45
          9 0 0 0 57                  ;50
          2 ?P                        ;52
          #xe1 #b10000100
          #xe1 #b00001011
          0 0 0 55])
        (*glx-string-table* glx-1))
    (should (equal (glx-get-string (glx-32 55)) "P"))
    (should (equal (glx-get-string (glx-32 53)) "ABCPABC"))))

(ert-deftest unicode-string-and-single-character-leaf-nodes ()
  "Unicode string and single character leaf nodes"
  :tags '(string)
  (let ((*glx-memory*
         [nil
          0 0 0 63                     ;4
          0 0 0 7                      ;8
          0 0 0 13                     ;12
          0 0 0 0 22 0 0 0 31          ;21
          0 0 0 0 40 0 0 0 53          ;30
          0 0 0 0 54 0 0 0 59          ;39
          5 0 0 0 ?é 0 0 34 30 0 0 0 0 ;49 é∞
          1                            ;53
          4 0 0 1 225                  ;58 ǡ
          4 0 0 34 149                 ;63 ⊕
          #xe1 #b00100000
          #xe1 #b10010011])
        (*glx-string-table* glx-1))
    (should (equal (glx-get-string (glx-32 64)) "é∞é∞"))
    (should (equal (glx-get-string (glx-32 66)) "⊕é∞ǡ"))))

(ert-deftest unknown-string-type ()
  "Unknown string type"
  :tags '(string)
  (let ((*glx-memory* [0]))
    (should-error (glx-get-string glx-0) :type 'glx-string-error)))

(ert-deftest use-terminator-node-for-compressed-string ()
  "Use terminator node for compressed string"
  :tags '(string)
  (let ((*glx-memory* [nil 0 0 0 13 0 0 0 1 0 0 0 13 45 #xe1])
        (*glx-string-table* glx-1))
    (should-error (glx-get-string (glx-32 14)) :type 'glx-string-error)))
