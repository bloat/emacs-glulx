#include <stdio.h>
#include <stdlib.h>
#include "glk.h"
#include "elisp_glk.h"

void glki_read_event_structure(event_t *event)
{
  event->type = get_next_glui32();
  event->win = get_next_pointer();
  event->val1 = get_next_glui32();
  event->val2 = get_next_glui32();
}

void glki_read_buffer_pointer_and_contents()
{
  char* buf = get_next_pointer();
  get_next_string(buf);
}

void glk_select(event_t *event)
{
  char* buf;
  printf("(glk-select)\n");
  glki_read_event_structure(event);
  if (event->type == evtype_LineInput)
  {
    glki_read_buffer_pointer_and_contents();
  }
}

void glk_request_line_event(winid_t win, char* buf, glui32 maxlen, glui32 initlen)
{
  printf("(glk-request-line-event (glki-pointer '%p) (glki-pointer '%p) (glki-glui32 %d) (glki-glui32 %d))\n", win, buf, maxlen, initlen);
  wait_for_return();
}

void glk_cancel_line_event(winid_t win, event_t *event)
{
  char* buf;
  event_t *some_event = malloc(sizeof(event_t));

  printf("(glk-cancel-line-event (glki-pointer '%p) (glki-pointer '%p)\n", win, event);
  glki_read_event_structure(event == NULL ? some_event : event);
  glki_read_buffer_pointer_and_contents();
  free(some_event);
}

void glk_request_char_event(winid_t win)
{
  printf("(glk-request-char-event (glki-pointer '%p))\n", win);
  wait_for_return();
}

void glk_cancel_char_event(winid_t win)
{
  printf("(glk-cancel-char-event (glki-pointer '%p))\n", win);
  wait_for_return();
}



