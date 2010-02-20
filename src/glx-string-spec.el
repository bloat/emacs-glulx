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

(context "Glulx Strings"
         (tag string)

         (specify "Decode a zero byte uncompressed string"
                  (let ((*glx-memory* [#xe0 0]))
                    (expect (glx-get-string glx-0) equals "")))

         (specify "Decode a two byte uncompressed string"
                  (let ((*glx-memory* [#xe0 65 66 0]))
                    (expect (glx-get-string glx-0) equals "AB")))

         (specify "Get the root node for a string table"
                  (let ((*glx-memory* [nil 0 0 0 13 0 0 0 1 0 0 0 14 1])
                        (*glx-string-table* glx-1))
                    (expect (glx-st-get-root-node-ptr) equals (glx-32 14))))

         (specify "Get a bitstream from a memory location2"
                  (let ((*glx-memory* [#b10101110 #b10101101]))
                    (let ((glx-bitstream (glx-get-bitstream glx-0)))
                      (expect (not (glx-next-bit glx-bitstream)))
                      (expect (glx-next-bit glx-bitstream))
                      (expect (glx-next-bit glx-bitstream))
                      (expect (glx-next-bit glx-bitstream))
                      (expect (not (glx-next-bit glx-bitstream)))
                      (expect (glx-next-bit glx-bitstream))
                      (expect (not (glx-next-bit glx-bitstream)))
                      (expect (glx-next-bit glx-bitstream))
                      (expect (glx-next-bit glx-bitstream))
                      (expect (not (glx-next-bit glx-bitstream))))))

         (specify "Use terminator node for compressed string"
                  (let ((*glx-memory* [nil 0 0 0 13 0 0 0 1 0 0 0 13 1 #xe1])
                        (*glx-string-table* glx-1))
                    (expect (glx-get-string (glx-32 14)) equals "")))

         (specify "Branch on branch node"
                  (let ((*glx-memory* [nil 0 0 0 23 0 0 0 3 0 0 0 13 0 0 0 0 22 0 0 0 23 1 1 #xe1 0 #xe1 1])
                        (*glx-string-table* glx-1))
                    (expect (glx-get-string (glx-32 24)) equals "")
                    (expect (glx-get-string '(0 0 0 26)) equals "")))

         (specify "Single character leaf node"
                  (let ((*glx-memory* [nil 0 0 0 24 0 0 0 3 0 0 0 13 0 0 0 0 22 0 0 0 24 2 ?A 1 #xe1 2 #xe1 1])
                        (*glx-string-table* glx-1))
                    (expect (glx-get-string (glx-32 25)) equals "A")
                    (expect (glx-get-string (glx-32 27)) equals "")))

         (specify "String leaf node"
                  (let ((*glx-memory* [nil 0 0 0 24 0 0 0 3 0 0 0 13 0 0 0 0 22 0 0 0 27 3 ?A ?B ?C 0 1 #xe1 2 #xe1 1])
                        (*glx-string-table* glx-1))
                    (expect (glx-get-string (glx-32 28)) equals "ABC")
                    (expect (glx-get-string (glx-32 30)) equals "")))

         (specify "String and single character leaf nodes"
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
                    (expect (glx-get-string (glx-32 50)) equals "ABCABC")
                    (expect (glx-get-string (glx-32 52)) equals "PABCJ")))

         (specify "Test indirect string reference"
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
                    (expect (glx-get-string (glx-32 55)) equals "P")
                    (expect (glx-get-string (glx-32 53)) equals "ABCPABC")))

         (specify "Test double indirect string reference"
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
                    (expect (glx-get-string (glx-32 55)) equals "P")
                    (expect (glx-get-string (glx-32 53)) equals "ABCPABC")))

         (specify "Unknown string type"
                  (let ((*glx-memory* [0]))
                    (expect (glx-get-string glx-0) throws glx-string-error)))

         (specify "Use terminator node for compressed string"
                  (let ((*glx-memory* [nil 0 0 0 13 0 0 0 1 0 0 0 13 45 #xe1])
                        (*glx-string-table* glx-1))
                    (expect (glx-get-string (glx-32 14)) throws glx-string-error))))
