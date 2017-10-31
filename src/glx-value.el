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

(require 'cl)

(defconst glx-b1 (* 256 256 256))
(defconst glx-b2 (* 256 256))
(defconst glx-b3 256)

(defconst glx-0 (list 0 0 0 0))

(put 'glx-value-error 'error-conditions '(error glx-error glx-value-error))
(put 'glx-value-error 'error-message "Glulx VM value error")

(defvar int->32 (make-hash-table))

(defsubst glx-int->32 (value)
  "glx-value private: create a glx-32 from an integer"
  (let ((memo (gethash value int->32)))
    (if memo
        memo
      (if (< value 0)
          (glx-- glx-0 (glx-32 (- value)))
        (let* ((b1 (/ value glx-b1))
               (b1-rem (% value glx-b1))
               (b2 (/ b1-rem glx-b2))
               (b2-rem (% b1-rem glx-b2))
               (b3 (/ b2-rem glx-b3))
               (b4 (% b2-rem glx-b3)))
          (glx-32 b4 b3 b2 b1))))))

(defsubst glx-32 (&optional byte-0 &optional byte-1 &optional byte-2 &optional byte-3)
  (if (and byte-0 (not byte-1) (not byte-2) (not byte-3))
      (glx-int->32 byte-0)
    (list (if byte-3 byte-3 0)
          (if byte-2 byte-2 0)
          (if byte-1 byte-1 0)
          (if byte-0 byte-0 0))))

(defconst glx-1 (glx-32 1))
(defconst glx-2 (glx-32 2))
(defconst glx-3 (glx-32 3))
(defconst glx-4 (glx-32 4))
(defconst glx-5 (glx-32 5))
(defconst glx-8 (glx-32 8))

(defsubst glx-+ (x y)
  (let ((result '())
        (carry 0))
    (dolist (digit (reverse (mapcar* #'+ x y)) result)
      (let ((new-digit (+ digit carry)))
        (setq carry 0)
        (if (< new-digit 256)
            (push new-digit result)
          (push (- new-digit 256) result)
          (setq carry 1))))))

(defun glx-- (x y)
  (let ((result '())
        (carry 0))
    (dolist (digit (reverse (mapcar* #'- x y)) result)
      (let ((new-digit (+ digit carry)))
        (setq carry 0)
        (if (>= new-digit 0)
            (push new-digit result)
          (push (+ 256 new-digit) result)
          (setq carry -1))))))

(defun glx-*-byte (value b)
  (let ((result '())
        (carry 0))
    (dolist (digit (reverse (mapcar #'(lambda (digit) (* digit b)) value)) result)
      (let ((new-digit (+ digit carry)))
        (setq carry 0)
        (if (< new-digit 256)
            (push new-digit result)
          (push (% new-digit 256) result)
          (setq carry (/ new-digit 256)))))))

(defun glx-lshift (value)
  (glx-32 0 (fourth value) (third value) (second value)))

(defun glx-* (x y)
  (let ((row1 (glx-*-byte x (fourth y)))
        (row2 (glx-lshift (glx-*-byte x (third y))))
        (row3 (glx-lshift (glx-lshift (glx-*-byte x (second y)))))
        (row4 (glx-lshift (glx-lshift (glx-lshift (glx-*-byte x (first y)))))))
    (glx-+ (glx-+ (glx-+ row1 row2) row3) row4)))

(defun glx-abs (x)
  (let* ((mask (glx-sshiftr x (glx-32 31))))
    (glx-- (glx-bitxor mask x) mask)))

(defun glx-subtraction-div (x y)
  "X and Y are unsigned glx-32 values. This is inefficient, so can't be used
for the main division algorithm."
  (let ((result 0))
    (while (or (glx-32-u< y x) (equal y x))
      (incf result)
      (setq x (glx-- x y)))
    (values result x)))

(defun glx-256-lshift-combine (x digit)
  "For long division, this takes a number and shifts its digits
 - radix 256, like a glx-32 - one place left, and adds the new digit
on to the right."
  (let ((xb (glx-32-get-bytes-as-list-big-endian x)))
    (glx-32 digit (fourth x) (third x) (second x))))

(defun glx-/ (x y)
  "Long division using the four radix 256 digits of a glx-32"
  (let* ((x-abs (glx-abs x))
         (y-abs (glx-abs y))
         (x-neg (not (equal x-abs x)))
         (y-neg (not (equal y-abs y)))
         (xb (glx-32-get-bytes-as-list-big-endian x-abs)))

    (multiple-value-bind (div1 rem1) (glx-subtraction-div (glx-32 (first xb)) y-abs)
      (multiple-value-bind (div2 rem2) (glx-subtraction-div (glx-256-lshift-combine rem1 (second xb)) y-abs)
        (multiple-value-bind (div3 rem3) (glx-subtraction-div (glx-256-lshift-combine rem2 (third xb)) y-abs)
          (multiple-value-bind (div4 rem4) (glx-subtraction-div (glx-256-lshift-combine rem3 (fourth xb)) y-abs)
            (let ((result (glx-32 div4 div3 div2 div1)))
              (values
               (if (and (not (and x-neg y-neg)) (or x-neg y-neg))
                   (glx-* (glx-32 -1) result)
                 result)
               (if x-neg
                   (glx-* (glx-32 -1) rem4)
                 rem4)))))))))

(defun glx-32->int (value)
  (let ((b1 (first value))
        (b2 (second value))
        (b3 (third value))
        (b4 (fourth value)))
    (if (> b1 15)
        (signal 'glx-value-error (list "32 bit value is too big" value))
      (+ b4 (* glx-b3 b3) (* glx-b2 b2) (* glx-b1 b1)))))

(defun glx-s32->int (value)
  (if (glx-neg-p value)
      (- (glx-32->int (glx-* (glx-32 -1) value)))
    (glx-32->int value)))

(defun glx-+1 (value)
  (glx-+ value glx-1))

(defun glx-0-p (value)
  (equal value glx-0))

(defun glx-neg-p (value)
  (< 127 (car value)))

(defun glx-pos-p (value)
  (and (not (glx-neg-p value))
       (not (glx-0-p value))))

(defun glx-truncate (value length)
  (apply #'glx-32 (reverse (subseq value (- length)))))

(defun glx-32-get-bytes-as-list-big-endian (value)
  value)

(defun glx-bitand (x y)
  (let ((xb (glx-32-get-bytes-as-list-big-endian x))
        (yb (glx-32-get-bytes-as-list-big-endian y)))
    (glx-32 (logand (fourth xb) (fourth yb))
            (logand (third xb) (third yb))
            (logand (second xb) (second yb))
            (logand (first xb) (first yb)))))

(defun glx-bitor (x y)
  (let ((xb (glx-32-get-bytes-as-list-big-endian x))
        (yb (glx-32-get-bytes-as-list-big-endian y)))
    (glx-32 (logior (fourth xb) (fourth yb))
            (logior (third xb) (third yb))
            (logior (second xb) (second yb))
            (logior (first xb) (first yb)))))

(defun glx-bitxor (x y)
  (let ((xb (glx-32-get-bytes-as-list-big-endian x))
        (yb (glx-32-get-bytes-as-list-big-endian y)))
    (glx-32 (logxor (fourth xb) (fourth yb))
            (logxor (third xb) (third yb))
            (logxor (second xb) (second yb))
            (logxor (first xb) (first yb)))))

(defun glx-bitnot (x)
  (let ((xb (glx-32-get-bytes-as-list-big-endian x)))
    (glx-32 (logand 255 (lognot (fourth xb)))
            (logand 255 (lognot (third xb)))
            (logand 255 (lognot (second xb)))
            (logand 255 (lognot (first xb))))))


(defun glx-shiftl-1 (x)
  (let ((xb (glx-32-get-bytes-as-list-big-endian x)))
    (glx-32 (logand 255 (lsh (fourth xb) 1))
            (+ (logand 255 (lsh (third xb) 1)) (if (zerop (logand 128 (fourth xb))) 0 1))
            (+ (logand 255 (lsh (second xb) 1)) (if (zerop (logand 128 (third xb))) 0 1))
            (+ (logand 255 (lsh (first xb) 1)) (if (zerop (logand 128 (second xb))) 0 1)))))

(defun glx-shiftl (x y)
  (if (glx-32-u> y (glx-32 31))
      glx-0
    (let ((result x))
      (dotimes (n (glx-32->int y) result)
        (setq result (glx-shiftl-1 result))))))

(defun glx-shiftr-1 (x p)
  (let ((xb (glx-32-get-bytes-as-list-big-endian x)))
    (glx-32 (+ (lsh (fourth xb) -1) (if (oddp (third xb)) 128 0))
            (+ (lsh (third xb) -1) (if (oddp (second xb)) 128 0))
            (+ (lsh (second xb) -1) (if (oddp (first xb)) 128 0))
            (+ p (lsh (first xb) -1)))))

(defun glx-sshiftr (x y)
  (let* ((xb (glx-32-get-bytes-as-list-big-endian x))
         (positive (zerop (logand 128 (first xb)))))
    (if (glx-32-u> y (glx-32 31))
        (if positive glx-0 (glx-32 255 255 255 255))
      (let ((result x))
        (dotimes (n (glx-32->int y) result)
          (setq result (glx-shiftr-1 result (if positive 0 128))))))))

(defun glx-ushiftr (x y)
  (if (glx-32-u> y (glx-32 31))
      glx-0
    (let ((result x))
      (dotimes (n (glx-32->int y) result)
        (setq result (glx-shiftr-1 result 0))))))

(defun glx-32->char (value)
  (fourth (glx-32-get-bytes-as-list-big-endian value)))

(defun glx-32-trunc (value bytes)
  (apply #'glx-32 (reverse (subseq (glx-32-get-bytes-as-list-big-endian value) 0 bytes))))

(defun glx-32-rand (limit)
  (cond ((glx-0-p limit) (glx-32 (random 256) (random 256) (random 256) (random 256)))
        (t (glx-32 (random (glx-s32->int limit))))))

(defun glx-32-u> (a b)
  (let ((a-bytes (glx-32-get-bytes-as-list-big-endian a))
        (b-bytes (glx-32-get-bytes-as-list-big-endian b)))
    (or (> (first a-bytes) (first b-bytes))
        (and (= (first a-bytes) (first b-bytes))
             (or (> (second a-bytes) (second b-bytes))
                 (and (= (second a-bytes) (second b-bytes))
                      (or (> (third a-bytes) (third b-bytes))
                          (and (= (third a-bytes) (third b-bytes))
                               (> (fourth a-bytes) (fourth b-bytes))))))))))

(defun glx-32-u< (a b)
  (and (not (glx-32-u> a b))
       (not (equal a b))))

(provide 'glx-value)
