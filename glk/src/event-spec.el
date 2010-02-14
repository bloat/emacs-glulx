(context "Events: "
         (tag events)

         (specify "glk-request-line-event primes window for input"
                  (unwind-protect
                      (progn
                        (glki-generate-new-window 'glk-wintype-text-buffer 'window 'stream)
                        (glk-request-line-event 'window "0x3456" 25 0)
                        (expect (glki-get-line-event-request 'window)))
                    (glki-dispose-window 'window)))

         (specify "glki-get-next-event should get the next event"
                  (let ((glk-event-queue '("go north" "eat fish")))
                    (expect (glki-get-next-event) equals "eat fish")
                    (expect (glki-get-next-event) equals "go north")
                    (expect (glki-get-next-event) equals nil)
                    (expect (glki-get-next-event) equals nil)))

         (specify "glki-add-event-to-queue should add an event to the queue"
                  (let ((glk-event-queue '())
                        (glk-select-waiting nil))
                    (glki-add-event-to-queue '(glk-evtype-lineinput window "go north"))
                    (expect glk-event-queue equals '((glk-evtype-lineinput window "go north")))))

         (specify "glki-add-event-to-queue should pass event to output stream if glk-select is waiting"
                  (let ((glk-select-waiting t)
                        (send-return-value-was-called))
                    (flet ((glki-send-return-value (a b) (setq send-return-value-was-called t)))
                      (glki-add-event-to-queue '(glk-evtype-lineinput window "go north"))
                      (expect send-return-value-was-called))))

         (specify "glki-create-line-input-event should create an event"
                  (unwind-protect
                      (progn
                        (glki-generate-new-window 'glk-wintype-text-buffer1 'window 'stream)
                        (setq glk-root-window 'window)
                        (save-excursion
                          (set-buffer (glki-opq-window-get-buffer 'window))
                          (let ((event (glki-create-line-input-event "go north" '0x3456)))
                            (expect event equals '(glk-evtype-lineinput window 8 0 0x3456 "go north"))
                            (expect (glki-get-line-event-window event) equals 'window))))
                    (glki-dispose-window 'window)
                    (setq glk-root-window nil)))

         (specify "glk-select should return the latest event"
                  (let ((glk-event-queue '((glk-evtype-lineinput window 8 0 0x3456 "go north"))))
                    (expect (glk-select) equals '(glk-evtype-lineinput window 8 0 0x3456 "go north"))))

         (specify "glk-select should signal no return value if there is no event"
                  (let ((glk-event-queue '())
                        (glk-select-waiting nil))
                    (expect (glk-select) equals 'glk-no-return)))

         (specify "glk-select should set flag to indicate event request is pending"
                  (let ((glk-event-queue '())
                        (glk-select-waiting nil))
                    (glk-select)
                    (expect glk-select-waiting)))

         (specify "glk-select should remove the line event request from the window"
                  (unwind-protect
                      (let ((glk-event-queue '((glk-evtype-lineinput window 8 0 0x3456 "go north"))))
                        (glki-add-line-event-request 'window '0x3456)
                        (glk-select)
                        (expect (not (glki-get-line-event-request 'window))))
                    (glki-dispose-window 'window)))

         (specify "glk-select should remove the char event request from the window"
                  (unwind-protect
                      (let ((glk-event-queue '((glk-evtype-charinput window 100 0))))
                        (put 'window 'glk-char-event t)
                        (glk-select)
                        (expect (not (glki-get-char-event-request 'window))))
                    (glki-dispose-window 'window)))

         (specify "glk-cancel-line-event when no event has been requested"
                  (expect (glk-cancel-line-event 'window) equals '(glk-evtype-none nil nil nil nil nil)))

         (specify "glk-cancel-line-event should return event data as if the player had hit return."
                  (flet ((glki-mode-add-input-to-event-queue () (glki-add-event-to-queue '(glk-evtype-lineinput window 5 0 0x3456 "go no"))))
                    (unwind-protect
                        (progn
                          (glki-generate-new-window 'glk-wintype-text-buffer 'window 'stream)
                          (glk-request-line-event 'window '0x3456 25 0)
                          (expect (glk-cancel-line-event 'window) equals '(glk-evtype-lineinput window 5 0 0x3456 "go no")))
                      (glki-dispose-window 'window))))

         (specify "glk-request-char-event primes window for input"
                  (unwind-protect
                      (progn
                        (glki-generate-new-window 'glk-wintype-text-buffer 'window 'stream)
                        (glk-request-char-event 'window)
                        (expect (glki-get-char-event-request 'window)))
                    (glki-dispose-window 'window)))

         (specify "glk-cancel-char-event should remove the char event request from the window"
                  (unwind-protect
                      (progn
                        (put 'window 'glk-char-event t)
                        (glk-cancel-char-event 'window)
                        (expect (not (glki-get-char-event-request 'window))))
                    (glki-dispose-window 'window))))
