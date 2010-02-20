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

(context "Machine operations, memory, etc."
         (tag glulx)

         (specify "Should be able to get a byte from memory"
                  (let ((*glx-memory* [45 245]))
                    (expect (glx-memory-get-byte glx-0) equals (glx-32 45))
                    (expect (glx-memory-get-byte glx-1) equals (glx-32 245))))

         (specify "Should be able to get a signed byte value from memory"
                  (let ((*glx-memory* [#x45 #x90]))
                    (expect (glx-memory-get-byte-signed glx-0) equals (glx-32 #x45))
                    (expect (glx-memory-get-byte-signed glx-1) equals (glx-32 -112))))

         (specify "Should be able to get a byte value from memory as an integer"
                  (let ((*glx-memory* [78 0]))
                    (expect (glx-memory-get-byte-int glx-0) equals 78)
                    (expect (glx-memory-get-byte-int glx-1) equals 0)))

         (specify "Should be able to get a 16 bit value from memory"
                  (let ((*glx-memory* [3 0 0 5 4 4]))
                    (expect (glx-memory-get-16 glx-1) equals glx-0)
                    (expect (glx-memory-get-16 glx-3) equals (glx-32 1284))))

         (specify "Should be able to get a signed 16 byte value from memory"
                  (let ((*glx-memory* [#x3d #x1c #xb9 #x9c]))
                    (expect (glx-memory-get-16-signed glx-0) equals (glx-32 #x3d1c))
                    (expect (glx-memory-get-16-signed glx-2) equals (glx-32 -18020))))

         (specify "Should be able to get a 32 bit value from memory"
                  (let ((*glx-memory* [3 0 0 5 4 4]))
                    (expect (glx-memory-get-32 glx-1) equals (glx-32 4 5))))

         (specify "Should be able to set a 32 bit value into memory"
                  (let ((*glx-memory* [3 0 0 5 4 4]))
                    (glx-memory-set glx-1 glx-3 4)
                    (expect *glx-memory* [3 0 0 0 3 4])))

         (specify "Should be able to set a 16 bit value into memory"
                  (let ((*glx-memory* [3 7 8 5 4 4]))
                    (glx-memory-set glx-1 (glx-32 4 5 6 7) 2)
                    (expect *glx-memory* [3 5 4 5 4 4])))

         (specify "Should be able to set an 8 bit value into memory"
                  (let ((*glx-memory* [3 7 8 5 4 4]))
                    (glx-memory-set glx-1 (glx-32 4 5 6 7) 2)
                    (expect *glx-memory* [3 4 8 5 4 4])))

         (specify "Linear search - find first key"
                  (let ((*glx-memory* [0 0 0 0]))
                    (expect (glx-memory-linear-search glx-0 glx-4 glx-0 glx-4 glx-1 glx-0 glx-0)
                            equals glx-0)))

         (specify "Linear search - find a subsequent key"
                  (let ((*glx-memory* [0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 3]))
                    (expect (glx-memory-linear-search glx-2 glx-4 glx-0 glx-4 glx-4 glx-0 glx-0)
                            equals glx-8)))

         (specify "Linear search - find an offset subsequent key"
                  (let ((*glx-memory* [0 0 0 0 0 0 0 0 0 1 0 0 0 0 2 0 0 0 0 3]))
                    (expect (glx-memory-linear-search glx-2 glx-4 glx-0 glx-5 glx-4 glx-1 glx-0)
                            equals (glx-32 10))))

         (specify "Linear search - key not found"
                  (let ((*glx-memory* [0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 3]))
                    (expect (glx-memory-linear-search glx-4 glx-4 glx-0 glx-4 glx-4 glx-0 glx-0)
                            equals glx-0)))

         (specify "Linear search - unbounded array"
                  (let ((*glx-memory* [0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 3]))
                    (expect (glx-memory-linear-search glx-2 glx-4 glx-0 glx-4 (glx-32 -1) glx-0 glx-0)
                            equals glx-8)))

         (specify "Linear search - small key"
                  (let ((*glx-memory* [0 0 0 1 0 2 0 3 0 4]))
                    (expect (glx-memory-linear-search glx-2 glx-2 glx-0 glx-2 glx-5 glx-0 glx-0)
                            equals glx-4)))

         (specify "Linear search - zero terminated"
                  (let ((*glx-memory* [0 0 0 1 0 0 0 0 0 0 0 2 0 0 0 3]))
                    (expect (glx-memory-linear-search glx-3 glx-4 glx-0 glx-4 glx-4 glx-0 glx-2)
                            equals glx-0)))

         (specify "Linear search - indirect key"
                  (let ((*glx-memory* [0 0 0 0 1 0 0 0 0 0 0 0 0 0 2 0 0 0 0 3]))
                    (expect (glx-memory-linear-search (glx-32 10) glx-5 glx-0 glx-5 glx-4 glx-0 glx-1)
                            equals (glx-32 10))))

         (specify "Linear search - return index"
                  (let ((*glx-memory* [0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 3]))
                    (expect (glx-memory-linear-search glx-2 glx-4 glx-0 glx-4 glx-4 glx-0 glx-4)
                            equals glx-2)))

         (specify "Linear search - return index failure"
                  (let ((*glx-memory* [0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 3]))
                    (expect (glx-memory-linear-search glx-5 glx-4 glx-0 glx-4 glx-4 glx-0 glx-4)
                            equals (glx-32 -1))))

         (specify "Binary search - find middle key"
                  (let ((*glx-memory* [0 0 0 3 0 0 0 0 0 0 0 4]))
                    (expect (glx-memory-binary-search glx-0 glx-4 glx-0 glx-4 glx-1 glx-0 glx-0)
                            equals glx-0)))

         (specify "Binary search - find a last key"
                  (let ((*glx-memory* [0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 3]))
                    (expect (glx-memory-binary-search glx-3 glx-4 glx-0 glx-4 glx-4 glx-0 glx-0)
                            equals (glx-32 12))))

         (specify "Binary search - find a first key"
                  (let ((*glx-memory* [0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 3]))
                    (expect (glx-memory-binary-search glx-0 glx-4 glx-0 glx-4 glx-4 glx-0 glx-0)
                            equals glx-0)))

         (specify "Store a string to memory"
                  (let ((*glx-memory* [0 0 0 0 0 0]))
                    (glx-memory-set-string glx-1 "Glulx")
                    (expect *glx-memory* equals [0 71 108 117 108 120])))

         (specify "Save undo"
                  (let ((*glx-memory* (make-vector 4 0))
                        (*glx-stack* (list (list nil (list (cons glx-0 glx-0)))))
                        (*glx-pc* (list 0 3 4 5))
                        (*glx-undo* nil))
                    (glx-save-undo)
                    (expect *glx-undo* equals '([0 0 0 0]
                                                ((nil (((0 0 0 0) 0 0 0 0))))
                                                (0 3 4 5)))
                    (glx-value-push glx-4)
                    (glx-memory-set glx-0 glx-5 4)
                    (expect *glx-undo* equals '([0 0 0 0]
                                                ((nil (((0 0 0 0) 0 0 0 0))))
                                                (0 3 4 5)))))

         (specify "Can't restore undo with no undo saved"
                  (let ((*glx-undo* nil))
                    (expect (glx-restore-undo) equals glx-1)))

         (specify "Restore undo"
                  (let ((*glx-undo* (list (make-vector 4 0)
                                          (list (list nil (list (cons glx-0 glx-0))))
                                          (list 0 3 4 5)))
                        (*glx-memory* nil)
                        (*glx-stack* nil)
                        (*glx-pc* nil))
                    (expect (glx-restore-undo) equals glx-0)
                    (expect *glx-undo* equals nil)
                    (expect *glx-memory* equals [0 0 0 0])
                    (expect *glx-stack* equals '((nil (((0 0 0 0) 0 0 0 0)))))
                    (expect *glx-pc* equals '(0 3 4 5)))))

