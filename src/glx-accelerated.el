;;; emacs glx
;;
;; Copyright (C) 2008, 2009, 2017, 2018 Andrew Cowper
;;
;; Author: Andrew Cowper (andrew.cowper@slothrop.net)
;;
;; URL: http://github.com/bloat/emacs-glulx
;;
;; This file is licensed under the terms of the GNU General Public
;; License as distributed with Emacs (press C-h C-c to view it).

(require 'glx-glulx)

(put 'glx-accelerated-error 'error-conditions '(error glx-error glx-accelerated-error))
(put 'glx-accelerated-error 'error-message "Glulx accelerated function error")

(defsubst glx-acc-param (n) (aref *glx-accelerated-parameters* n))

(defun glx-accelerated-util-obj-in-class (obj)
  (equal (glx-memory-get-32 (glx-+ (glx-acc-param 7) (glx-+ obj (glx-32 13))))
         (glx-acc-param 2)))

(defun glx-acclerated-util-get-prop-old (obj id)
  (let (result
        (cla glx-0))
    (when (not (glx-0-p (glx-bitand id (glx-32 0 0 255 255))))
      (setq cla (glx-memory-get-32 (glx-+ (glx-acc-param 0)
                                          (glx-*-byte (glx-bitand id (glx-32 255 255)) 4))))
      (if (glx-0-p (glx-accelerated-oc-cl-5 obj cla))
          (setq result glx-0)
        (setq id (glx-ushiftr id (glx-32 16)))
        (setq obj cla)))
    (if result
        result
      (let ((prop (glx-accelerated-cp-tab-2 obj id)))
        (if (or (glx-0-p prop)
                (and (glx-accelerated-util-obj-in-class obj)
                     (glx-0-p cla)
                     (or (glx-32-u< id (glx-acc-param 1))
                         (not (glx-32-u< id (glx-+ glx-8 (glx-acc-param 1))))))
                (and (not (equal (glx-memory-get-32 (glx-acc-param 6)) obj))
                     (not (= 0 (logand 1 (glx-memory-get-byte-int (glx-+ (glx-32 9) prop)))))))
            glx-0
          prop)))))

(defun glx-acclerated-util-get-prop-new (obj id)
  (let (result
        (cla glx-0))
    (when (not (glx-0-p (glx-bitand id (glx-32 0 0 255 255))))
      (setq cla (glx-memory-get-32 (glx-+ (glx-acc-param 0)
                                          (glx-*-byte (glx-bitand id (glx-32 255 255)) 4))))
      (if (glx-0-p (glx-accelerated-oc-cl-11 obj cla))
          (setq result glx-0)
        (setq id (glx-ushiftr id (glx-32 16)))
        (setq obj cla)))
    (if result
        result
      (let ((prop (glx-accelerated-cp-tab-8 obj id)))
        (if (or (glx-0-p prop)
                (and (glx-accelerated-util-obj-in-class obj)
                     (glx-0-p cla)
                     (or (glx-32-u< id (glx-acc-param 1))
                         (not (glx-32-u< id (glx-+ glx-8 (glx-acc-param 1))))))
                (and (not (equal (glx-memory-get-32 (glx-acc-param 6)) obj))
                     (not (= 0 (logand 1 (glx-memory-get-byte-int (glx-+ (glx-32 9) prop)))))))
            glx-0
          prop)))))

(defun glx-accelerated-z-region-1 (memptr32)
  (if (glx-neg-p memptr32)
      glx-0
    (let ((memptr (glx-32->int memptr32)))
      (if (and (>= memptr 36) (< memptr (length *glx-memory*)))
          (let ((val (glx-memory-get-byte-int memptr32)))
            (cond ((>= val #xe0) glx-3)
                  ((>= val #xc0) glx-2)
                  ((and (>= val #x70) (<= val #x7f) (>= memptr (glx-32->int *glx-ram-start*))) glx-1)
                  (glx-0)))
        glx-0))))

(defun glx-accelerated-cp-tab-2 (obj id)
  (when (not (equal glx-1 (glx-accelerated-z-region-1 obj)))
    (signal 'glx-accelerated-error (list "CP tab was not given an object at" obj)))
  (let ((otab (glx-memory-get-32 (glx-+ obj (glx-32 16)))))
    (if (glx-0-p otab)
        glx-0
      (glx-memory-binary-search id glx-2 (glx-+ otab glx-4) (glx-32 10) (glx-memory-get-32 otab) glx-0 glx-0))))

(defun glx-accelerated-ra-pr-3 (obj id)
  (let ((prop (glx-acclerated-util-get-prop-old obj id)))
    (if (glx-0-p prop)
        glx-0
      (glx-memory-get-32 (glx-+ prop glx-4)))))

(defun glx-accelerated-rl-pr-4 (obj id)
  (let ((prop (glx-acclerated-util-get-prop-old obj id)))
    (if (glx-0-p prop)
        glx-0
      (glx-*-byte (glx-memory-get-16 (glx-+ prop glx-2)) 4))))

(defun glx-accelerated-oc-cl-5 (obj cla)
  (let ((zr (glx-accelerated-z-region-1 obj)))
    (cond ((equal zr glx-3) (if (equal (glx-acc-param 5) cla) glx-1 glx-0))
          ((equal zr glx-2) (if (equal (glx-acc-param 4) cla) glx-1 glx-0))
          ((not (equal zr glx-1)) glx-0)
          ((equal (glx-acc-param 2) cla)
           (if (or (glx-accelerated-util-obj-in-class obj)
                   (equal (glx-acc-param 2) obj)
                   (equal (glx-acc-param 5) obj)
                   (equal (glx-acc-param 4) obj)
                   (equal (glx-acc-param 3) obj))
               glx-1
             glx-0))
          ((equal (glx-acc-param 3) cla)
           (if (or (glx-accelerated-util-obj-in-class obj)
                   (equal (glx-acc-param 2) obj)
                   (equal (glx-acc-param 5) obj)
                   (equal (glx-acc-param 4) obj)
                   (equal (glx-acc-param 3) obj))
               glx-0
             glx-1))
          ((or (equal (glx-acc-param 5) cla)
               (equal (glx-acc-param 4) cla))
           glx-0)
          ((not (glx-accelerated-util-obj-in-class cla))
           (signal 'glx-accelerated-error (list "Tried to apply ofclass with non-class" obj cla)))
          ((let ((inlist (glx-accelerated-ra-pr-3 obj glx-2)))
              (if (glx-0-p inlist)
                  glx-0
                (let ((result glx-0)
                      (inlistlen (glx-32->int (glx-accelerated-rl-pr-4 obj glx-2)))
                      (jx 0))
                  (while (and (< jx inlistlen) (glx-0-p result))
                    (if (equal cla (glx-memory-get-32 (glx-+ inlist (glx-32 (* 4 jx)))))
                        (setq result glx-1)
                      (incf jx)))
                  result)))))))

(defun glx-accelerated-rv-pr-6 (obj id)
  (let ((addr (glx-accelerated-ra-pr-3 obj id)))
    (if (glx-0-p addr)
        (if (and (glx-pos-p id) (glx-32-u< id (glx-acc-param 1)))
            (glx-memory-get-32 (glx-+ (glx-acc-param 8) (glx-*-byte id 4)))
          (signal 'glx-accelerated-error (list "Tried to read (something)" obj id)))
      (glx-memory-get-32 addr))))

(defun glx-accelerated-op-pr-7 (obj id)
  (let ((zr (glx-accelerated-z-region-1 obj)))
    (cond ((equal zr glx-3)
           (if (or (equal id (glx-+ (glx-acc-param 1) (glx-32 6)))
                   (equal id (glx-+ (glx-acc-param 1) (glx-32 7))))
               glx-1
             glx-0))
          ((equal zr glx-2)
           (if (equal id (glx-+ (glx-acc-param 1) glx-5))
               glx-1
             glx-0))
          ((not (equal zr glx-1)) glx-0)
          ((and (not (glx-neg-p (glx-- id (glx-acc-param 1))))
                (glx-neg-p (glx-- id (glx-+ (glx-acc-param 1) glx-8)))
                (glx-accelerated-util-obj-in-class obj))
           glx-1)
          ((not (glx-0-p (glx-accelerated-ra-pr-3 obj id))) glx-1)
          (glx-0))))

(defun glx-accelerated-cp-tab-8 (obj id)
  (when (not (equal glx-1 (glx-accelerated-z-region-1 obj)))
    (signal 'glx-accelerated-error (list "CP tab was not given an object at" obj)))
  (multiple-value-bind (div rem) (glx-/ (glx-acc-param 7) glx-4)
    (let ((otab (glx-memory-get-32 (glx-+ obj (glx-*-byte (glx-+ glx-3 div) 4)))))
      (if (glx-0-p otab)
          glx-0
        (glx-memory-binary-search id glx-2 (glx-+ otab glx-4) (glx-32 10) (glx-memory-get-32 otab) glx-0 glx-0)))))

(defun glx-accelerated-ra-pr-9 (obj id)
  (let ((prop (glx-acclerated-util-get-prop-new obj id)))
    (if (glx-0-p prop)
        glx-0
      (glx-memory-get-32 (glx-+ prop glx-4)))))

(defun glx-accelerated-rl-pr-10 (obj id)
  (let ((prop (glx-acclerated-util-get-prop-new obj id)))
    (if (glx-0-p prop)
        glx-0
      (glx-*-byte (glx-memory-get-16 (glx-+ prop glx-2)) 4))))

(defun glx-accelerated-oc-cl-11 (obj cla)
  (let ((zr (glx-accelerated-z-region-1 obj)))
    (cond ((equal zr glx-3) (if (equal (glx-acc-param 5) cla) glx-1 glx-0))
          ((equal zr glx-2) (if (equal (glx-acc-param 4) cla) glx-1 glx-0))
          ((not (equal zr glx-1)) glx-0)
          ((equal (glx-acc-param 2) cla)
           (if (or (glx-accelerated-util-obj-in-class obj)
                   (equal (glx-acc-param 2) obj)
                   (equal (glx-acc-param 5) obj)
                   (equal (glx-acc-param 4) obj)
                   (equal (glx-acc-param 3) obj))
               glx-1
             glx-0))
          ((equal (glx-acc-param 3) cla)
           (if (or (glx-accelerated-util-obj-in-class obj)
                   (equal (glx-acc-param 2) obj)
                   (equal (glx-acc-param 5) obj)
                   (equal (glx-acc-param 4) obj)
                   (equal (glx-acc-param 3) obj))
               glx-0
             glx-1))
          ((or (equal (glx-acc-param 5) cla)
               (equal (glx-acc-param 4) cla))
           glx-0)
          ((not (glx-accelerated-util-obj-in-class cla))
           (signal 'glx-accelerated-error (list "Tried to apply ofclass with non-class" obj cla)))
          ((let ((inlist (glx-accelerated-ra-pr-9 obj glx-2)))
              (if (glx-0-p inlist)
                  glx-0
                (let ((result glx-0)
                      (inlistlen (glx-32->int (glx-accelerated-rl-pr-10 obj glx-2)))
                      (jx 0))
                  (while (and (< jx inlistlen) (glx-0-p result))
                    (if (equal cla (glx-memory-get-32 (glx-+ inlist (glx-32 (* 4 jx)))))
                        (setq result glx-1)
                      (incf jx)))
                  result)))))))

(defun glx-accelerated-rv-pr-12 (obj id)
  (let ((addr (glx-accelerated-ra-pr-9 obj id)))
    (if (glx-0-p addr)
        (if (and (glx-pos-p id) (glx-32-u< id (glx-acc-param 1)))
            (glx-memory-get-32 (glx-+ (glx-acc-param 8) (glx-*-byte id 4)))
          (signal 'glx-accelerated-error (list "Tried to read (something)" obj id)))
      (glx-memory-get-32 addr))))

(defun glx-accelerated-op-pr-13 (obj id)
  (let ((zr (glx-accelerated-z-region-1 obj)))
    (cond ((equal zr glx-3)
           (if (or (equal id (glx-+ (glx-acc-param 1) (glx-32 6)))
                   (equal id (glx-+ (glx-acc-param 1) (glx-32 7))))
               glx-1
             glx-0))
          ((equal zr glx-2)
           (if (equal id (glx-+ (glx-acc-param 1) glx-5))
               glx-1
             glx-0))
          ((not (equal zr glx-1)) glx-0)
          ((and (not (glx-neg-p (glx-- id (glx-acc-param 1))))
                (glx-neg-p (glx-- id (glx-+ (glx-acc-param 1) glx-8)))
                (glx-accelerated-util-obj-in-class obj))
           glx-1)
          ((not (glx-0-p (glx-accelerated-ra-pr-9 obj id))) glx-1)
          (glx-0))))

(provide 'glx-accelerated)
