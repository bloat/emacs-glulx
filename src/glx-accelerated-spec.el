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
