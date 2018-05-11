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

(ert-deftest glk-request-line-event-primes-window-for-input ()
  "glk-request-line-event primes window for input"
  :tags '(glk event)
  (unwind-protect
      (let ((window (glki-generate-new-window 'glk-wintype-text-buffer 'window 'stream 0)))
        (glk-request-line-event window "0x3456" 25 0)
        (should (glki-get-line-event-request window)))
    (glki-kill-all-windows)))

(ert-deftest glki-get-next-event-should-get-the-next-event ()
  "glki-get-next-event should get the next event"
  :tags '(glk event)
  (let ((glk-event-queue '("go north" "eat fish")))
    (should (equal (glki-get-next-event) "eat fish"))
    (should (equal (glki-get-next-event) "go north"))
    (should-not (glki-get-next-event))
    (should-not (glki-get-next-event))))

(ert-deftest glki-add-event-to-queue-should-add-an-event-to-the-queue ()
  "glki-add-event-to-queue should add an event to the queue"
  :tags '(glk event)
  (let ((glk-event-queue '())
        (glk-select-waiting nil))
    (glki-add-event-to-queue '(glk-evtype-lineinput window 8 0 0x3456 "go north"))
    (should (equal glk-event-queue '((glk-evtype-lineinput window 8 0 0x3456 "go north"))))))

(ert-deftest glki-create-line-input-event-should-create-an-event ()
  "glki-create-line-input-event should create an event"
  :tags '(glk event)
  (unwind-protect
      (let ((window (glki-generate-new-window 'glk-wintype-text-buffer 'window 'stream 0)))
        (setq glk-root-window window)
        (with-current-buffer (glki-opq-window-buffer window)
          (let ((event (glki-create-line-input-event "go north" '0x3456)))
            (should (equal event (list 'glk-evtype-lineinput window 8 0 '0x3456 "go north")))
            (should (eq (glki-get-line-event-window event) window)))))
    (glki-kill-all-windows)
    (setq glk-root-window nil)))

(ert-deftest glk-select-should-return-the-latest-event ()
  "glk-select should return the latest event"
  :tags '(glk event)
  (let* ((window (glki-generate-new-window 'glk-wintype-text-buffer 'window 'stream 0))
         (event (list 'glk-evtype-lineinput window 8 0 '0x3456 "go north"))
         (glk-event-queue (list event)))
    (should (equal (glk-select) event))))

(ert-deftest glk-select-should-signal-no-return-value-if-there-is-no-event ()
  "glk-select should signal no return value if there is no event"
  :tags '(glk event)
  (let ((glk-event-queue '())
        (glk-select-waiting nil))
    (should (equal (glk-select) 'glx-return-to-emacs))))

(ert-deftest glk-select-should-set-flag-to-indicate-event-request-is-pending ()
  "glk-select should set flag to indicate event request is pending"
  :tags '(glk event)
  (let ((glk-event-queue '())
        (glk-select-waiting nil))
    (glk-select)
    (should glk-select-waiting)))

(ert-deftest glk-select-should-remove-the-line-event-request-from-the-window ()
  "glk-select should remove the line event request from the window"
  :tags '(glk event)
  (unwind-protect
      (let* ((window (glki-generate-new-window 'glk-wintype-text-buffer 'window 'stream 0))
             (glk-event-queue (list (list 'glk-evtype-lineinput window 8 0 '0x3456 "go north"))))
        (glki-add-line-event-request window '0x3456)
        (glk-select)
        (should-not (glki-get-line-event-request window)))
    (glki-kill-all-windows)))

(ert-deftest glk-select-should-remove-the-char-event-request-from-the-window ()
  "glk-select should remove the char event request from the window"
  :tags '(glk event)
  (unwind-protect
      (let* ((window (glki-generate-new-window 'glk-wintype-text-buffer 'window 'stream 0))
             (glk-event-queue (list (list 'glk-evtype-charinput window 100 0))))
        (setf (glki-opq-window-glk-char-event window) t)
        (glk-select)
        (should-not (glki-get-char-event-request window)))
    (glki-kill-all-windows)))

(ert-deftest glk-cancel-line-event-when-no-event-has-been-requested ()
  "glk-cancel-line-event when no event has been requested"
  :tags '(glk event)
  (let ((window (glki-generate-new-window 'glk-wintype-text-buffer 'window 'stream 0)))
    (should (equal (glk-cancel-line-event window) '(glk-evtype-none nil nil nil nil nil)))))

(ert-deftest glk-cancel-line-event-should-return-event-data-as-if-the-player-had-hit-return ()
  "glk-cancel-line-event should return event data as if the player had hit return."
  :tags '(glk event)
  (unwind-protect
      (cl-letf (((symbol-function 'glki-mode-add-input-to-event-queue) (lambda () (glki-add-event-to-queue '(glk-evtype-lineinput window 5 0 0x3456 "go no")))))
        (let ((window (glki-generate-new-window 'glk-wintype-text-buffer 'window 'stream 0)))
          (glk-request-line-event window '0x3456 25 0)
          (should (equal (glk-cancel-line-event window) '(glk-evtype-lineinput window 5 0 0x3456 "go no")))))
    (glki-kill-all-windows)))

(ert-deftest glk-request-char-event-primes-window-for-input ()
  "glk-request-char-event primes window for input"
  :tags '(glk event)
  (unwind-protect
      (let ((window (glki-generate-new-window 'glk-wintype-text-buffer 'window 'stream 0)))
        (glk-request-char-event window)
        (should (glki-get-char-event-request window)))
    (glki-kill-all-windows)))

(ert-deftest glk-cancel-char-event-should-remove-the-char-event-request-from-the-window ()
  "glk-cancel-char-event should remove the char event request from the window"
  :tags '(glk event)
  (unwind-protect
      (let ((window (glki-generate-new-window 'glk-wintype-text-buffer 'window-id 'stream-id 0)))
        (setf (glki-opq-window-glk-char-event window) t)
        (glk-cancel-char-event window)
        (should-not (glki-get-char-event-request window)))
    (glki-kill-all-windows)))
