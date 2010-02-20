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

(context "Loading the game file: "
         (tag loading)

         (specify "glx-load-story-file should return a vector"
                  (expect (vectorp (glx-load-story-file "/home/bloat/dev/src/lisp/elisp/glulx/advent.ulx"))))

         (specify "First byte loaded should be 0x47"
                  (expect (aref (glx-load-story-file "/home/bloat/dev/src/lisp/elisp/glulx/advent.ulx") 0)
                          equals #x47))

         (specify "Second byte loaded should be 0x6c"
                  (expect (aref (glx-load-story-file "/home/bloat/dev/src/lisp/elisp/glulx/advent.ulx") 1)
                          equals #x6c))

         (specify "Four hundred and fifty third byte loaded should be 0xeb"
                  (expect (aref (glx-load-story-file "/home/bloat/dev/src/lisp/elisp/glulx/advent.ulx") 453)
                          equals #xeb))

         (specify "Five hundred and forty second byte loaded should be 0xff"
                  (expect (aref (glx-load-story-file "/home/bloat/dev/src/lisp/elisp/glulx/advent.ulx") 542)
                          equals #xff))

         (specify "Should reject non glulx files"
                  (let ((*glx-memory* (make-vector 32 0)))
                    (expect (glx-process-header) throws glx-load-error)))

         (specify "Should reject glulx files with the wrong version"
                  (let ((*glx-memory* (vconcat [#x47 #x6c #x75 #x6c 0 1 0 0] (make-vector 24 0))))
                    (expect (glx-process-header) throws glx-load-error)))

         (specify "Should set ram-start"
                  (let ((*glx-memory* (vconcat [#x47 #x6c #x75 #x6c 0 2 0 0 0 5 6 0] (make-vector 20 0)))
                        (*glx-ram-start* nil))
                    (glx-process-header)
                    (expect *glx-ram-start* equals (glx-32 0 6 5))))

         (specify "Should extend memory map"
                  (let ((*glx-memory* [#x47 #x6c #x75 #x6c
                                            0 3 0 0
                                            0 5 6 0
                                            0 0 0 36
                                            0 0 0 44
                                            0 0 0 0 0 0 0 0 0 0 0 0]))
                    (glx-process-header)
                    (expect *glx-memory* equals [#x47 #x6c #x75 #x6c
                                                      0 3 0 0
                                                      0 5 6 0
                                                      0 0 0 36
                                                      0 0 0 44
                                                      0 0 0 0 0 0 0 0 0 0 0 0
                                                      0 0 0 0 0 0 0 0])))

         (specify "Should set string encoding table"
                  (let ((*glx-memory* [#x47 #x6c #x75 #x6c
                                            0 3 1 0
                                            0 5 6 0
                                            0 0 0 24
                                            0 0 0 32
                                            0 0 0 0
                                            0 0 0 0
                                            0 3 4 0]))
                    (glx-process-header)
                    (expect *glx-string-table* equals (glx-32 0 4 3))))

         (specify "Should return the start function pointer"
                  (let ((*glx-memory* [#x47 #x6c #x75 #x6c
                                            0 2 0 0
                                            0 5 6 0
                                            0 0 0 24
                                            0 0 0 32
                                            0 0 0 0
                                            0 0 5 6
                                            0 3 4 0]))
                    (expect (glx-process-header) equals (glx-32 6 5))
                    (expect *glx-string-table* equals (glx-32 0 4 3)))))

