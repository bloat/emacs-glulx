EMACS=emacs -q -batch --no-site-file --eval
GLK=../glk/src

glx-value.elc: glx-value.el
	${EMACS} "(progn (add-to-list 'load-path \".\") (byte-compile-file \"glx-value.el\"))"

glx-glulx.elc: glx-glulx.el glx-value.elc
	${EMACS} "(progn (add-to-list 'load-path \".\") (byte-compile-file \"glx-glulx.el\"))"

glx-stack.elc: glx-stack.el glx-glulx.elc
	${EMACS} "(progn (add-to-list 'load-path \".\") (byte-compile-file \"glx-stack.el\"))"

glx-string.elc: glx-string.el glx-value.elc glx-glulx.elc
	${EMACS} "(progn (add-to-list 'load-path \".\") (byte-compile-file \"glx-string.el\"))"

glx-load.elc: glx-load.el glx-value.elc glx-glulx.elc glx-stack.elc
	${EMACS} "(progn (add-to-list 'load-path \".\") (add-to-list 'load-path \"${GLK}\") (byte-compile-file \"glx-load.el\"))"

glx-glk.elc: glx-glk.el glx-value.elc glx-glulx.elc glx-stack.elc glx-load.elc glx-exec.elc
	${EMACS} "(progn (add-to-list 'load-path \".\") (add-to-list 'load-path \"${GLK}\") (byte-compile-file \"glx-glk.el\"))"

glx-exec.elc: glx-exec.el glx-stack.elc
	${EMACS} "(progn (add-to-list 'load-path \".\") (byte-compile-file \"glx-exec.el\"))"

glx-instructions.elc: glx-instructions.el glx-value.elc glx-glulx.elc glx-exec.elc glx-string.elc glx-glk.elc
	${EMACS} "(progn (add-to-list 'load-path \".\") (add-to-list 'load-path \"${GLK}\") (byte-compile-file \"glx-instructions.el\"))"

glx-compile.elc: glx-compile.el glx-instructions.elc
	${EMACS} "(progn (add-to-list 'load-path \".\") (byte-compile-file \"glx-compile.el\"))"

glx-accelerated.elc: glx-accelerated.el glx-glulx.elc
	${EMACS} "(progn (add-to-list 'load-path \".\") (byte-compile-file \"glx-accelerated.el\"))"

all: glx-value.elc glx-glulx.elc glx-stack.elc glx-exec.elc glx-instructions.elc glx-value.elc glx-string.elc glx-glk.elc glx-load.elc glx-compile.elc glx-accelerated.elc

