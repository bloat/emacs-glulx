(context "Function dispatch: "
         (tag dispatch)

         (macrolet ((with-glk-function-call-expectation
                     (arg-list fun-body fun-call)
                     `(flet ((glk-function ,arg-list (expect ,@fun-body)))
                        (glki-call-function nil ,fun-call))))

           (specify "Should call function asked for (glk-function)"
                    (expect
                     (flet ((glk-function () 'mocked))
                       (glki-call-function nil "(glk-function)"))
                     equals 'mocked))

           (specify "Should call function asked for (glk-another-function)"
                    (expect
                     (flet ((glk-another-function () 'mocked))
                       (glki-call-function nil "(glk-another-function)"))
                     equals 'mocked))

           (specify "Should call function with glui32"
                    (with-glk-function-call-expectation (x) (x equals 4) "(glk-function (glki-glui32 4))"))

           (specify "Should call function with char"
                    (with-glk-function-call-expectation (x) (x equals ?H) "(glk-function (glki-char \"H\"))"))

           (specify "Should call function with wintype"
                    (with-glk-function-call-expectation
                     (a b c d e f)
                     ((list a b c d e f) equals '(glk-wintype-all-types
                                                  glk-wintype-pair
                                                  glk-wintype-blank
                                                  glk-wintype-text-buffer
                                                  glk-wintype-text-grid
                                                  glk-wintype-graphics))
                     "(glk-function (glki-wintype 0) (glki-wintype 1) (glki-wintype 2) (glki-wintype 3) (glki-wintype 4) (glki-wintype 5))"))

           (specify "Should call function with nil winid for nil"
                    (with-glk-function-call-expectation (a) (a equals nil) "(glk-function (glki-pointer '(nil)))"))

           (specify "Should call function with winid"
                    (with-glk-function-call-expectation (a) (a equals '0x345663) "(glk-function (glki-pointer '0x345663))"))

           (specify "Should call function with winmethod"
                    (with-glk-function-call-expectation (a b) ((list a b) equals '((glk-winmethod-right glk-winmethod-fixed) (glk-winmethod-left glk-winmethod-proportional)))
                                                        "(glk-function (glki-winmethod 17) (glki-winmethod 32))"))

           (specify "Should call function with string"
                    (with-glk-function-call-expectation (a) (a equals "testing") "(glk-function (glki-string \"testing\"))"))

           (specify "glki-format-glui32 should format a glui32"
                    (expect (glki-format-glui32 1234) equals "0041234"))

           (specify "glki-format-pointer should format a pointer"
                    (expect (glki-format-pointer '0x3456789) equals "0090x3456789")
                    (expect (glki-format-pointer '()) equals "004NULL"))

           (specify "glki-get-formatters should return correct formatters"
                    (expect (glki-get-formatters 'glk-window-open) equals '(glki-format-pointer))
                    (expect (glki-get-formatters 'glk-put-string) equals '(glki-void-formatter)))

           (specify "glki-format-results should format results correctly"
                    (expect (glki-format-results 'glk-window-open '0x23432) equals "0070x23432\n")
                    (expect (glki-format-results 'glk-put-string nil) equals "\n")
                    (expect (glki-format-results 'glk-select-waiting '(glk-evtype-lineinput 0x3b3b3 5 1 0xbff7a650 "go north"))
                            equals "0013\n0070x3b3b3\n0015\n0011\n0100xbff7a650\n008go north\n")
                    (expect (glki-format-results 'glk-select '(glk-evtype-lineinput 0x3b3b3 5 1 0xbff7a650 "go north"))
                            equals "0013\n0070x3b3b3\n0015\n0011\n0100xbff7a650\n008go north\n")
                    (expect (glki-format-results 'glk-window-open '()) equals "004NULL\n")
                    (expect (glki-format-results 'glk-cancel-line-event '(glk-evtype-none nil 0 0 nil ""))
                            equals "0010\n004NULL\n0010\n0010\n004NULL\n000\n"))))
