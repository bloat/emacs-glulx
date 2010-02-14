(context "GLK Mode: "
         (tag mode)

         (macrolet ((with-glk-window (&body body)
                                     `(unwind-protect
                                          (progn
                                            (glki-generate-new-window 'glk-wintype-text-buffer 'window 'stream)
                                            (setq glk-root-window 'window)
                                            ,@body)
                                        (kill-buffer "*glk*")
                                        (glki-dispose-window 'window)
                                        (setq glk-root-window nil))))

           (specify "glki-mode-add-input-to-event-queue should do nothing if no event is requested"
                    (with-glk-window
                     (save-current-buffer
                       (set-buffer (glki-opq-window-get-buffer 'window))
                       (insert "> go north")
                       (glki-mode-add-input-to-event-queue))
                     (expect (not (glki-get-next-event)))))

           (specify "glki-mode-add-input-to-event-queue should add the last line of test typed to the event queue"
                    (with-glk-window
                     (glk-request-line-event 'window '0x3456 100 0)
                     (save-current-buffer
                       (set-buffer (glki-opq-window-get-buffer 'window))
                       (insert (propertize "> " 'read-only "Game text is read only" 'rear-nonsticky t 'front-sticky t 'glk-text t))
                       (insert "go north")
                       (glki-mode-add-input-to-event-queue))
                     (expect (glki-get-next-event) equals '(glk-evtype-lineinput window 8 0 0x3456 "go north"))))

           (specify "glki-mode-add-input-to-event-queue should deal with different prompts."
                    (with-glk-window
                     (glk-request-line-event 'window '0x3456 100 0)
                     (save-current-buffer
                       (set-buffer (glki-opq-window-get-buffer 'window))
                       (insert (propertize "What next? " 'read-only "Game text is read only" 'rear-nonsticky t 'front-sticky t 'glk-text t))
                       (insert "go north")
                       (glki-mode-add-input-to-event-queue))
                     (expect (glki-get-next-event) equals '(glk-evtype-lineinput window 8 0 0x3456 "go north"))))

           (specify "glki-mode-add-input-to-event-queue should deal with empty input."
                    (with-glk-window
                     (glk-request-line-event 'window '0x3456 100 0)
                     (save-current-buffer
                       (set-buffer (glki-opq-window-get-buffer 'window))
                       (insert (propertize "What next? " 'read-only "Game text is read only" 'rear-nonsticky t 'front-sticky t 'glk-text t))
                       (glki-mode-add-input-to-event-queue))
                     (expect (glki-get-next-event) equals '(glk-evtype-lineinput window 0 0 0x3456 ""))))

           (specify "glki-press-any-key should do nothing if no character input is requested."
                    (with-glk-window
                     (glki-press-any-key)
                     (expect (not (glki-get-next-event)))))

           (specify "glki-press-any-key should add a char event to the queue"
                    (with-glk-window
                     (glk-request-char-event 'window)
                     (setq last-command-char 100)
                     (save-current-buffer
                       (set-buffer (glki-opq-window-get-buffer 'window))
                       (glki-press-any-key)
                       (expect (glki-get-next-event) equals '(glk-evtype-charinput window 100 0)))))))
