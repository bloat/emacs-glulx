#include <stdio.h>
#include <stdlib.h>
#include "glk.h"
#include "elisp_glk.h"

void glk_put_string(char *s)
{
  char *escaped = escape_backslashes(s);
  printf("(glk-put-string (glki-string \"%s\"))\n", escaped);
  free(escaped);
  wait_for_return();
}

void glk_put_string_stream(strid_t str, char *s)
{
  char *escaped = escape_backslashes(s);
  printf("(glk-put-string-stream (glki-pointer '%p) (glki-string \"%s\"))\n", str, escaped);
  free(escaped);
  wait_for_return();
}

void glk_put_char (unsigned char ch)
{
  printf("(glk-put-char (glki-format-char %d))\n", ch);
  wait_for_return();
}

void glk_put_char_stream (strid_t str, unsigned char ch)
{
  printf("(glk-put-char-stream (glki-pointer '%p) (glki-format-char %d))\n", str, ch);
  wait_for_return();
}

strid_t glk_stream_open_file(frefid_t fileref, glui32 fmode, glui32 rock)
{
  strid_t strid = malloc(sizeof(stream_t));

  printf("(glk-stream-open-file (glki-pointer '%p) (glki-glui32 %d) (glki-glui32 %d) (glki-pointer '%p))\n", fileref, fmode, rock, strid);
  wait_for_return();
  return strid;
}

glsi32 glk_get_char_stream(strid_t str)
{
  printf("(glk-get-char-stream (glki-pointer '%p))\n", str);
  return get_next_glsi32();
}

glui32 glk_get_buffer_stream(strid_t str, char *buf, glui32 len)
{
  printf("(glk-get-buffer-stream (glki-pointer '%p) (glki-glui32 %d))\n", str, len);
  get_next_string(buf);
  return get_next_glui32();
}

void glk_set_style(glui32 val)
{
  // Noop
}
