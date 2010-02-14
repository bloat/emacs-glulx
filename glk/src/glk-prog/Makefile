LIB_SOURCE=../glk-c-lib

hello_elisp_glk: elisp_glk_main.o hello_elisp_glk.o $(LIB_SOURCE)/libelispglk.a
	gcc -L$(LIB_SOURCE) -o $@ elisp_glk_main.o hello_elisp_glk.o -lelispglk

elisp_glk_main.o: $(LIB_SOURCE)/glk.h $(LIB_SOURCE)/elisp_glk_main.c
	gcc -c -o $@ $(LIB_SOURCE)/elisp_glk_main.c

hello_elisp_glk.o: $(LIB_SOURCE)/glk.h hello_elisp_glk.c
	gcc -I$(LIB_SOURCE) -c -o $@ hello_elisp_glk.c

clean:
	rm -f *.o hello_elisp_glk
