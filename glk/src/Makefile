EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs-x86_64-10_9 -q -batch --no-site-file --eval

glk-base.elc: glk-base.el
	${EMACS} "(progn (add-to-list 'load-path \".\") (byte-compile-file \"glk-base.el\"))"

glk-customize.elc: glk-customize.el
	${EMACS} "(progn (add-to-list 'load-path \".\") (byte-compile-file \"glk-customize.el\"))"

glk-encoding.elc: glk-encoding.el
	${EMACS} "(progn (add-to-list 'load-path \".\") (byte-compile-file \"glk-encoding.el\"))"

glk-opaque.elc: glk-opaque.el
	${EMACS} "(progn (add-to-list 'load-path \".\") (byte-compile-file \"glk-opaque.el\"))"

glk-file.elc: glk-file.el glk-opaque.elc
	${EMACS} "(progn (add-to-list 'load-path \".\") (byte-compile-file \"glk-file.el\"))"

glk-stream.elc: glk-stream.el glk-opaque.elc glk-file.elc
	${EMACS} "(progn (add-to-list 'load-path \".\") (byte-compile-file \"glk-stream.el\"))"

glk-window.elc: glk-window.el glk-opaque.elc glk-stream.elc
	${EMACS} "(progn (add-to-list 'load-path \".\") (byte-compile-file \"glk-window.el\"))"

glk-event.elc: glk-event.el glk-window.elc glk-base.elc
	${EMACS} "(progn (add-to-list 'load-path \".\") (byte-compile-file \"glk-event.el\"))"

glk.elc: glk.el glk-window.elc
	${EMACS} "(progn (add-to-list 'load-path \".\") (byte-compile-file \"glk.el\"))"

all: glk-customize.elc glk.elc glk-window.elc glk-event.elc glk-stream.elc glk-opaque.elc glk-customize.elc glk-base.elc glk-encoding.elc glk-file.elc

clean:
	rm -v *.elc

