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

(cl-macrolet ((with-glk-window (&body body)
                               `(unwind-protect
                                    (progn
                                      (glki-generate-new-window 'glk-wintype-text-buffer 'window 'stream 42)
                                      (setq glk-root-window 'window)
                                      ,@body)
                                  (kill-buffer "*glk*")
                                  (glki-dispose-window 'window)
                                  (setq glk-root-window nil))))
  
  (ert-deftest glki-mode-add-input-to-event-queue-should-do-nothing-if-no-event-is-requested ()
    "glki-mode-add-input-to-event-queue should do nothing if no event is requested"
    :tags '(glk mode)
    (with-glk-window
     (save-current-buffer
       (set-buffer (glki-opq-window-get-buffer 'window))
       (insert "> go north")
       (glki-mode-add-input-to-event-queue))
     (should-not (glki-get-next-event))))

  (ert-deftest glki-mode-add-input-to-event-queue-should-add-the-last-line-of-test-typed-to-the-event-queue ()
    "glki-mode-add-input-to-event-queue should add the last line of test typed to the event queue"
    :tags '(glk mode)
    (with-glk-window
     (glk-request-line-event 'window '0x3456 100 0)
     (save-current-buffer
       (set-buffer (glki-opq-window-get-buffer 'window))
       (insert (propertize "> " 'read-only "Game text is read only" 'rear-nonsticky t 'front-sticky t 'glk-text t))
       (insert "go north")
       (glki-mode-add-input-to-event-queue))
     (should (equal (glki-get-next-event) '(glk-evtype-lineinput window 8 0 0x3456 "go north")))))

  (ert-deftest glki-mode-add-input-to-event-queue-should-deal-with-different-prompts ()
    "glki-mode-add-input-to-event-queue should deal with different prompts."
    :tags '(glk mode)
    (with-glk-window
     (glk-request-line-event 'window '0x3456 100 0)
     (save-current-buffer
       (set-buffer (glki-opq-window-get-buffer 'window))
       (insert (propertize "What next? " 'read-only "Game text is read only" 'rear-nonsticky t 'front-sticky t 'glk-text t))
       (insert "go north")
       (glki-mode-add-input-to-event-queue))
     (should (equal (glki-get-next-event) '(glk-evtype-lineinput window 8 0 0x3456 "go north")))))

  (ert-deftest glki-mode-add-input-to-event-queue-should-deal-with-empty-input ()
    "glki-mode-add-input-to-event-queue should deal with empty input."
    :tags '(glk mode)
    (with-glk-window
     (glk-request-line-event 'window '0x3456 100 0)
     (save-current-buffer
       (set-buffer (glki-opq-window-get-buffer 'window))
       (insert (propertize "What next? " 'read-only "Game text is read only" 'rear-nonsticky t 'front-sticky t 'glk-text t))
       (glki-mode-add-input-to-event-queue))
     (should (equal (glki-get-next-event) '(glk-evtype-lineinput window 0 0 0x3456 "")))))

  (ert-deftest glki-press-any-key-should-do-nothing-if-no-character-input-is-requested ()
    "glki-press-any-key should do nothing if no character input is requested."
    :tags '(glk mode)
    (with-glk-window
     (glki-press-any-key)
     (should-not (glki-get-next-event))))

  (ert-deftest glki-press-any-key-should-add-a-char-event-to-the-queue ()
    "glki-press-any-key should add a char event to the queue"
    :tags '(glk mode)
    (with-glk-window
     (glk-request-char-event 'window)
     (setq last-command-event 100)
     (save-current-buffer
       (set-buffer (glki-opq-window-get-buffer 'window))
       (glki-press-any-key)
       (should (equal (glki-get-next-event) '(glk-evtype-charinput window 100 0)))))))
