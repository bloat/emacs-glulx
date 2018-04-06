.PHONY: glk glulx

glk:
	$(MAKE) -C glk/src clean all

glulx:
	$(MAKE) -C src clean all

all: glk glulx
