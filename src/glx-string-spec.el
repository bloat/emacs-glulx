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

(defmacro check-string (expected-result expected-call-count memptr)
  (declare (indent 2))
  (let ((result (gensym))
        (call-count (gensym)))
    `(let ((,result "")
           (,call-count 0)
           (*glx-iosys* (list (lambda (c) (incf ,call-count) (setq ,result (concat ,result (list c)))) glx-0 glx-2)))
       (glx-get-string ,memptr #'glx-call-function-and-return-to-emacs)
       (should (equal ,result ,expected-result))
       (should (= ,call-count ,expected-call-count)))))

(ert-deftest decode-a-zero-byte-uncompressed-string ()
  "Decode a zero byte uncompressed string"
  :tags '(string)

  (let ((*glx-memory* [#xe0 0]))
    (check-string "" 0 glx-0)))

(ert-deftest decode-a-two-byte-uncompressed-string ()
  "Decode a two byte uncompressed string"
  :tags '(string)
  (let ((*glx-memory* [#xe0 65 66 0]))
    (check-string "AB" 2 glx-0)))

(ert-deftest get-the-root-node-for-a-string-table ()
  "Get the root node for a string table"
  :tags '(string)
  (let ((*glx-memory* [nil 0 0 0 13 0 0 0 1 0 0 0 14 1])
        (*glx-string-table* glx-1))
    (should (equal (glx-st-get-root-node-ptr) (glx-32 14)))))

(ert-deftest get-a-bitstream-from-a-memory-location ()
  "Get a bitstream from a memory location"
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
    (check-string "" 0 (glx-32 14))))

(ert-deftest branch-on-branch-node ()
  "Branch on branch node"
  :tags '(string)
  (let ((*glx-memory* [nil 0 0 0 23 0 0 0 3 0 0 0 13 0 0 0 0 22 0 0 0 23 1 1 #xe1 0 #xe1 1])
        (*glx-string-table* glx-1))
    (check-string "" 0 (glx-32 24))
    (check-string "" 0 (glx-32 26))))

(ert-deftest single-character-leaf-node ()
  "Single character leaf node"
  :tags '(string)
  (let ((*glx-memory* [nil 0 0 0 24
                           0 0 0 3
                           0 0 0 13
                           0 0 0 0 22 0 0 0 24
                           2 ?A ;22
                           1    ;24
                           #xe1 2
                           #xe1 1])
        (*glx-string-table* glx-1))

    (check-string "A" 1 (glx-32 25))
    (check-string "" 0 (glx-32 27))))

(ert-deftest string-leaf-node ()
  "String leaf node"
  :tags '(string)
  (let ((*glx-memory* [nil 0 0 0 24
                           0 0 0 3
                           0 0 0 13
                           0 0 0 0 22 0 0 0 27
                           3 ?A ?B ?C 0 ;22
                           1            ;27
                           #xe1 2
                           #xe1 1])
        (*glx-string-table* glx-1))
    (check-string "ABC" 3 (glx-32 28))
    (check-string "" 0 (glx-32 30))))

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
    (check-string "ABCABC" 6 (glx-32 50))
    (check-string "PABCJ" 5 (glx-32 52))))

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
    (check-string "P" 1 (glx-32 55))
    (check-string "ABCPABC" 7 (glx-32 53))))

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
    (check-string "P" 1 (glx-32 55))
    (check-string "ABCPABC" 7 (glx-32 53))))

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
    (check-string "é∞é∞" 4 (glx-32 64))
    (check-string "⊕é∞ǡ" 4 (glx-32 66))))

(ert-deftest unknown-string-type ()
  "Unknown string type"
  :tags '(string)
  (let ((*glx-memory* [0]))
    (should-error (glx-get-string glx-0 #'glx-call-function-and-return-to-emacs) :type 'glx-string-error)))

(ert-deftest use-terminator-node-for-compressed-string ()
  "Use terminator node for compressed string"
  :tags '(string)
  (let ((*glx-memory* [nil 0 0 0 13 0 0 0 1 0 0 0 13 45 #xe1])
        (*glx-string-table* glx-1))
    (should-error (glx-get-string (glx-32 14) #'glx-call-function-and-return-to-emacs) :type 'glx-string-error)))

(ert-deftest test-indirect-function-reference ()
  "Test indirect function reference"
  :tags '(string)

  (cl-letf ((called-memptr nil)
            (called-args nil)
            ((symbol-function 'glx-call-function-and-return-to-emacs) (lambda (memptr args)
                                                                        (setq called-memptr memptr)
                                                                        (setq called-args args))))
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
            #xc0])
          (*glx-string-table* glx-1))
      (check-string "ABCABC" 6 (glx-32 53))
      (should (equal called-memptr (glx-32 55)))
      (should-not called-args))))

(ert-deftest test-double-indirect-function-reference ()
  "Test double indirect function reference"
  :tags '(string)

  (cl-letf ((called-memptr nil)
            (called-args nil)
            ((symbol-function 'glx-call-function-and-return-to-emacs) (lambda (memptr args)
                                                                        (setq called-memptr memptr)
                                                                        (setq called-args args))))
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
            9 0 0 0 56                  ;50
            2 ?P                        ;52
            #xe1 #b10000100
            #xc0 
            0 0 0 55])
          (*glx-string-table* glx-1))
      (check-string "ABCABC" 6 (glx-32 53))
      (should (equal called-memptr (glx-32 55)))
      (should-not called-args))))

(ert-deftest test-indirect-string-reference-with-args ()
  "Test indirect string reference with args"
  :tags '(string)
  (let ((*glx-memory*
         [nil
          0 0 0 52                    ;4
          0 0 0 7                     ;8
          0 0 0 13                    ;12
          0 0 0 0 22 0 0 0 31         ;21
          0 0 0 0 40 0 0 0 45         ;30
          0 0 0 0 46 0 0 0 63         ;39
          3 ?A ?B ?C 0                ;44
          1                           ;45
          10 0 0 0 67 0 0 0 2 0 0 0 4 0 0 0 8 ;62
          2 ?P                        ;64
          #xe1 #b10000100
          #xe1 #b00001011])
        (*glx-string-table* glx-1))
    (check-string "P" 1 (glx-32 67))
    (check-string "ABCPABC" 7 (glx-32 65))))

(ert-deftest test-double-indirect-string-reference-with-args ()
  "Test double indirect string reference with args"
  :tags '(string)
  (let ((*glx-memory*
         [nil
          0 0 0 52                    ;4
          0 0 0 7                     ;8
          0 0 0 13                    ;12
          0 0 0 0 22 0 0 0 31         ;21
          0 0 0 0 40 0 0 0 45         ;30
          0 0 0 0 46 0 0 0 63         ;39
          3 ?A ?B ?C 0                ;44
          1                           ;45
          11 0 0 0 69 0 0 0 2 0 0 0 4 0 0 0 8 ;62
          2 ?P                        ;64
          #xe1 #b10000100
          #xe1 #b00001011
          0 0 0 67])
        (*glx-string-table* glx-1))
    (check-string "P" 1 (glx-32 67))
    (check-string "ABCPABC" 7 (glx-32 65))))

(ert-deftest test-indirect-function-reference-with-args ()
  "Test indirect function reference with args"
  :tags '(string)

  (cl-letf ((called-memptr nil)
            (called-args nil)
            ((symbol-function 'glx-call-function-and-return-to-emacs) (lambda (memptr args)
                                                                        (setq called-memptr memptr)
                                                                        (setq called-args args))))
    (let ((*glx-memory*
           [nil
            0 0 0 52                    ;4
            0 0 0 7                     ;8
            0 0 0 13                    ;12
            0 0 0 0 22 0 0 0 31         ;21
            0 0 0 0 40 0 0 0 45         ;30
            0 0 0 0 46 0 0 0 63         ;39
            3 ?A ?B ?C 0                ;44
            1                           ;45
            10 0 0 0 67 0 0 0 2 0 0 0 4 0 0 0 8 ;62
            2 ?P                        ;64
            #xe1 #b10000100
            #xc0])
          (*glx-string-table* glx-1))
      (check-string "ABCABC" 6 (glx-32 65))
      (should (equal called-memptr (glx-32 67)))
      (should (equal called-args `(,glx-4 ,glx-8))))))

(ert-deftest test-double-indirect-function-reference-with-args ()
  "Test double indirect function reference with args"
  :tags '(string)

  (cl-letf ((called-memptr nil)
            (called-args nil)
            ((symbol-function 'glx-call-function-and-return-to-emacs) (lambda (memptr args)
                                                                        (setq called-memptr memptr)
                                                                        (setq called-args args))))
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
            11 0 0 0 68 0 0 0 2 0 0 0 4 0 0 0 8 ;62
            2 ?P                        ;64
            #xe1 #b10000100
            #xc0 
            0 0 0 67])
          (*glx-string-table* glx-1))
      (check-string "ABCABC" 6 (glx-32 65))
      (should (equal called-memptr (glx-32 67)))
      (should (equal called-args `(,glx-4 ,glx-8))))))

