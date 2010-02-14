OBJ=elisp_glk_window.o elisp_glk_stream.o elisp_glk_event.o elisp_glk_results.o elisp_glk_encoding.o elisp_glk_structure.o elisp_glk_file.o elisp_glk_util.o

libelispglk.a: $(OBJ)
	ar rvcs $@ $(OBJ)

elisp_glk_window.o: elisp_glk_window.c glk.h elisp_glk.h

elisp_glk_stream.o: elisp_glk_stream.c glk.h elisp_glk.h

elisp_glk_stream.o: elisp_glk_stream.c glk.h elisp_glk.h

elisp_glk_event.o: elisp_glk_event.c glk.h elisp_glk.h

elisp_glk_encoding.o: elisp_glk_encoding.c glk.h elisp_glk.h

elisp_glk_results.o: elisp_glk_results.c elisp_glk.h

elisp_glk_structure.o: elisp_glk_structure.c glk.h

elisp_glk_file.o: elisp_glk_file.c glk.h

elisp_glk_util.o: elisp_glk_util.c

clean:
	rm *.o libelispglk.a

