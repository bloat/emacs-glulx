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

(ert-deftest load-story-file ()
  "glx-load-story-file should return a vector"
  :tags '(loading)
  (should (vectorp (glx-load-story-file (expand-file-name "~/Downloads/advent.ulx")))))

(ert-deftest load-glulx-file-1 ()
  "First byte loaded should be 0x47"
  :tags '(loading)
  (should (= (aref (glx-load-story-file (expand-file-name "~/Downloads/advent.ulx")) 0) #x47)))

(ert-deftest load-glulx-file-2 ()
  "Second byte loaded should be 0x6c"
  :tags '(loading)
  (should (= (aref (glx-load-story-file (expand-file-name "~/Downloads/advent.ulx")) 1) #x6c)))

(ert-deftest load-glulx-file-3 ()
  "Four hundred and fifty third byte loaded should be 0xeb"
  :tags '(loading)
  (should (= (aref (glx-load-story-file (expand-file-name "~/Downloads/advent.ulx")) 453) #xeb)))

(ert-deftest load-glulx-file-4 ()
  "Five hundred and forty second byte loaded should be 0xff"
  :tags '(loading)
  (should (= (aref (glx-load-story-file (expand-file-name "~/Downloads/advent.ulx")) 542) #xff)))

(ert-deftest reject-non-glulx-file ()
  "Should reject non glulx files"
  :tags '(loading)
  (let ((*glx-memory* (make-vector 32 0)))
    (should-error (glx-process-header) :type 'glx-load-error)))

(ert-deftest reject-wrong-version ()
  "Should reject glulx files with the wrong version"
  :tags '(loading)
  (let ((*glx-memory* (vconcat [#x47 #x6c #x75 #x6c 0 1 0 0] (make-vector 24 0))))
    (should-error (glx-process-header) :type 'glx-load-error)))

(ert-deftest set-ram-start ()
  "Should set ram-start"
  :tags '(loading)
  (let ((*glx-memory* (vconcat [#x47 #x6c #x75 #x6c 0 2 0 0 0 5 6 0] (make-vector 20 0)))
        (*glx-ram-start* nil))
    (glx-process-header)
    (should (equal *glx-ram-start* (glx-32 0 6 5)))))

(ert-deftest extend-memory-map ()
  "Should extend memory map"
  (let ((*glx-memory* [#x47 #x6c #x75 #x6c
                            0 3 0 0
                            0 5 6 0
                            0 0 0 36
                            0 0 0 44
                            0 0 0 0 0 0 0 0 0 0 0 0]))
    (glx-process-header)
    (should (equal *glx-memory* [#x47 #x6c #x75 #x6c
                                      0 3 0 0
                                      0 5 6 0
                                      0 0 0 36
                                      0 0 0 44
                                      0 0 0 0 0 0 0 0 0 0 0 0
                                      0 0 0 0 0 0 0 0]))))

(ert-deftest set-encoding-table ()
  "Should set string encoding table"
  (let ((*glx-memory* [#x47 #x6c #x75 #x6c
                            0 3 1 0
                            0 5 6 0
                            0 0 0 24
                            0 0 0 32
                            0 0 0 0
                            0 0 0 0
                            0 3 4 0]))
    (glx-process-header)
    (should (equal *glx-string-table* (glx-32 0 4 3)))))

(ert-deftest start-function-pointer ()
  "Should return the start function pointer"
  (let ((*glx-memory* [#x47 #x6c #x75 #x6c
                            0 2 0 0
                            0 5 6 0
                            0 0 0 24
                            0 0 0 32
                            0 0 0 0
                            0 0 5 6
                            0 3 4 0]))
    (should (equal (glx-process-header) (glx-32 6 5)))
    (should (equal *glx-string-table* (glx-32 0 4 3)))))

