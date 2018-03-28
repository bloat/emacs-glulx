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

(ert-deftest z-region-header-false ()
  "Returns false if the arg points to the header"
  :tags '(accelerated)
  
  (should (glx-0-p (glx-accelerated-z-region (glx-32 20)))))

(ert-deftest z-region-outside-memory-false ()
  "Returns false if the arg points outside memory"
  :tags '(accelerated)

  (let ((*glx-memory* (make-vector 40 0)))
    (should (glx-0-p (glx-accelerated-z-region (glx-32 50))))))

(ert-deftest z-region-string-table ()
  "Returns 3 if the arg points to a string table"
  :tags '(accelerated)

  (let ((*glx-memory* (vconcat (make-vector 40 0) [#xe0 0])))
    (should (equal (glx-accelerated-z-region (glx-32 40)) glx-3))))

(ert-deftest z-region-function ()
  "Returns 2 if the arg points to a function"
  :tags '(accelerated)

  (let ((*glx-memory* (vconcat (make-vector 40 0) [#xc0 0])))
    (should (equal (glx-accelerated-z-region (glx-32 40)) glx-2))))

(ert-deftest z-region-object ()
  "Returns 1 if the arg points to an object or class"
  :tags '(accelerated)

  (let ((*glx-memory* (vconcat (make-vector 40 0) [#x75 0]))
        (*glx-ram-start* (glx-32 38)))
    (should (equal (glx-accelerated-z-region (glx-32 40)) glx-1))))

(ert-deftest cp-tab-error ()
  "Signals error if argument is not an object"
  :tags '(accelerated)
  
  (cl-letf (((symbol-function 'glx-accelerated-z-region) (lambda (x) (should (equal x glx-5)) glx-2)))
    (should-error (glx-accelerated-cp-tab glx-5 (glx-32 10)) :type 'glx-accelerated-error)))

(ert-deftest cp-tab ()
  "Does binary search with appropriate arguments"
  :tags '(accelerated)

  (let (search-args
        (*glx-ram-start* (glx-32 37))
        (*glx-memory* (vconcat (make-vector 40 0) [0 0 0 0 #x70 0 0 0 100 0 0 0 0 0 0 0 0 0 0 0 0 0 0 45])))
    (cl-letf (((symbol-function 'glx-memory-binary-search) (lambda (&rest args) (setq search-args args) (glx-32 100))))
      (should (equal (glx-accelerated-cp-tab (glx-32 44) (glx-32 10)) (glx-32 100)))
      (should (equal search-args (list (glx-32 10) glx-2 (glx-32 49) (glx-32 10) (glx-32 100) glx-0 glx-0))))))
