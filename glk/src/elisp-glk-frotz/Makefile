LIB_SOURCE=../glk-c-lib
BLORB_SOURCE=../blorb

elisp_frotz: buffer.o err.o fastmem.o files.o glkmisc.o glkscreen.o input.o main.o math.o object.o process.o quetzal.o random.o redirect.o sound.o stream.o table.o text.o variable.o $(LIB_SOURCE)/libelispglk.a elisp_glk_main.o
	gcc -L$(LIB_SOURCE) -o $@ buffer.o err.o fastmem.o files.o glkmisc.o glkscreen.o input.o main.o math.o object.o process.o quetzal.o random.o redirect.o sound.o stream.o table.o text.o variable.o elisp_glk_main.o -lelispglk

buffer.o: buffer.c frotz.h
	gcc -c -o $@ buffer.c

err.o: err.c frotz.h
	gcc -c -o $@ err.c

fastmem.o: fastmem.c $(LIB_SOURCE)/glk.h glkio.h frotz.h $(BLORB_SOURCE)/gi_blorb.h
	gcc -c -o $@ -I $(LIB_SOURCE) -I $(BLORB_SOURCE) fastmem.c

files.o: files.c $(LIB_SOURCE)/glk.h glkio.h frotz.h
	gcc -c -o $@ -I $(LIB_SOURCE) files.c

glkmisc.o: glkmisc.c glkfrotz.h $(LIB_SOURCE)/glk.h
	gcc -c -o $@ -I $(LIB_SOURCE) glkmisc.c

glkscreen.o: glkscreen.c glkfrotz.h $(LIB_SOURCE)/glk.h
	gcc -c -o $@ -I $(LIB_SOURCE) glkscreen.c

input.o: input.c frotz.h
	gcc -c -o $@ input.c

main.o: main.c frotz.h $(LIB_SOURCE)/glk.h
	gcc -c -o $@ -I $(LIB_SOURCE) main.c

math.o: math.c frotz.h
	gcc -c -o $@ math.c

object.o: object.c frotz.h
	gcc -c -o $@ object.c

process.o: process.c frotz.h
	gcc -c -o $@ process.c

quetzal.o: quetzal.c $(LIB_SOURCE)/glk.h glkio.h frotz.h
	gcc -c -o $@ -I $(LIB_SOURCE) quetzal.c

random.o: random.c frotz.h
	gcc -c -o $@ random.c

redirect.o: redirect.c frotz.h
	gcc -c -o $@ redirect.c

sound.o: sound.c frotz.h
	gcc -c -o $@ redirect.c

stream.o: stream.c frotz.h
	gcc -c -o $@ stream.c

table.o: table.c frotz.h
	gcc -c -o $@ table.c

text.o: text.c frotz.h
	gcc -c -o $@ text.c

variable.o: variable.c frotz.h
	gcc -c -o $@ variable.c

elisp_glk_main.o: $(LIB_SOURCE)/glk.h $(LIB_SOURCE)/elisp_glk_main.c
	gcc -c -o $@ $(LIB_SOURCE)/elisp_glk_main.c

clean:
	rm -f *.o elisp_frotz
