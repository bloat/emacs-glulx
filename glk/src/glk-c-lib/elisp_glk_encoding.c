#include <stdio.h>
#include "glk.h"
#include "elisp_glk.h"

unsigned char glk_char_to_lower(unsigned char ch)
{
  printf("(glk-char-to-lower (glki-char \"%c\"))\n", ch);
  return get_next_char();
}
