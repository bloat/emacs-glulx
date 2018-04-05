;;; emacs glx  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2008, 2009 Andrew Cowper
;;
;; Author: Andrew Cowper (andrew.cowper@slothrop.net)
;;
;; URL: http://github.com/bloat/emacs-glulx
;;
;; This file is licensed under the terms of the GNU General Public
;; License as distributed with Emacs (press C-h C-c to view it).

(defconst glx-b1 (* 256 256 256))
(defconst glx-b2 (* 256 256))
(defconst glx-b3 256)
(defconst glx-max (- (lsh 1 32) 1))

(defconst glx-most-positive 2147483647)
(defconst glx-most-negative -2147483648)

(defconst glx-emacs-no-bits
  (let ((count 2) ;; don't really understand why we need two extra here.
        (n most-positive-fixnum))
    (while (> n 1)
      (setq n (lsh n -1))
      (cl-incf count))
    count))

(put 'glx-value-error 'error-conditions '(error glx-error glx-value-error))
(put 'glx-value-error 'error-message "Glulx VM value error")

(defsubst glx-32 (&optional byte-0 byte-1 byte-2 byte-3)
  (logand 4294967295 (+ (or byte-0 0)
                        (* 256 (or byte-1 0))
                        (* 256 256 (or byte-2 0))
                        (* 256 256 256 (or byte-3 0)))))

(defconst glx-0 (glx-32))
(defconst glx-1 (glx-32 1))
(defconst glx-2 (glx-32 2))
(defconst glx-3 (glx-32 3))
(defconst glx-4 (glx-32 4))
(defconst glx-5 (glx-32 5))
(defconst glx-8 (glx-32 8))

(defsubst glx-s32->int (value) (if (> value glx-most-positive) (- value (+ 1 glx-max)) value))
(defsubst glx-32->int (value) value)

(defsubst glx-+ (x y) (glx-32 (+ x y)))
(defsubst glx-- (x y) (glx-32 (- x y)))
(defsubst glx-* (x y) (glx-32 (* x y)))
(defsubst glx-abs (x) (abs (glx-s32->int x)))
(defsubst glx-/ (x y) (list (glx-32 (/ (glx-s32->int x) (glx-s32->int y))) (glx-32 (% (glx-s32->int x) (glx-s32->int y)))))

(defsubst glx-+1 (value) (glx-32 (1+ value)))
(defsubst glx-0-p (value) (zerop value))
(defsubst glx-neg-p (value) (< (glx-s32->int value) 0))
(defsubst glx-pos-p (value) (> (glx-s32->int value) 0))
(defsubst glx-truncate (value length) (logand value (- (lsh 1 (* 8 length)) 1)))
(defsubst glx-32-get-bytes-as-list-big-endian (value) (list (logand (lsh value -24) #xff) 
                                                            (logand (lsh value -16) #xff)
                                                            (logand (lsh value -8) #xff)
                                                            (logand value #xff)))
(defsubst glx-bitand (x y) (logand x y))
(defsubst glx-bitor (x y) (logior x y))
(defsubst glx-bitxor (x y) (logxor x y))
(defsubst glx-bitnot (x) (logand glx-max (lognot x)))

(defsubst glx-shiftl (x y) (glx-32 (lsh x y)))
(defsubst glx-sshiftr (x y) (lsh (ash (lsh x (- glx-emacs-no-bits 32)) (- y)) (- 32 glx-emacs-no-bits)))
(defsubst glx-ushiftr (x y) (lsh x (- y)))

(defsubst glx-32->char (value) (nth 3 (glx-32-get-bytes-as-list-big-endian value)))

(defsubst glx-32->unicode-char (value) (decode-char 'unicode (glx-32->int value)))

(defsubst glx-32-lo-trunc (value bytes) (logand value (- (lsh 1 (* 8 bytes)) 1)))
(defsubst glx-32-hi-trunc (value bytes) (lsh value (* 8 (- bytes 4))))

(defun glx-32-rand (limit)
  (cond ((glx-0-p limit) (glx-32 (random 256) (random 256) (random 256) (random 256)))
        ((glx-neg-p limit) (glx-32 (* -1 (random (glx-32->int (glx-abs limit))))))
        (t (glx-32 (random (glx-s32->int limit))))))

(defsubst glx-32-u> (a b) (> a b))
(defsubst glx-32-u< (a b) (< a b))

(defsubst glx-32->dec-string (x) (number-to-string (glx-s32->int x)))

(provide 'glx-value)
