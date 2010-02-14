#include <stdio.h>
#include <stdlib.h>
#include "glk.h"
#include "elisp_glk.h"

winid_t glk_window_open(winid_t splitwin, glui32 method, glui32 size,
  glui32 wintype, glui32 rock)
{
  winid_t winid = malloc(sizeof(window_t));
  strid_t strid = malloc(sizeof(stream_t));
  winid_t parentid;
  strid_t parent_streamid;
  winid_t returned_winid;

  if (splitwin == NULL)
  {
    parentid = NULL;
    parent_streamid = NULL;
  }
  else
  {
    parentid = malloc(sizeof(window_t));
    parent_streamid = malloc(sizeof(stream_t));
  }

  printf("(glk-window-open (glki-pointer '%p) (glki-winmethod %d) (glki-glui32 %d) (glki-wintype %d) (glki-glui32 %d) (glki-pointer '%p) (glki-pointer '%p) (glki-pointer '%p) (glki-pointer '%p))\n",
         splitwin, method, size, wintype, rock, winid, parentid, strid, parent_streamid);

  returned_winid = get_next_pointer();

  if (returned_winid == NULL)
  {
    free(winid);
    return NULL;
  }
  else
  {
    return winid;
  }
}

void glk_set_window(winid_t win)
{
  printf("(glk-set-window (glki-pointer '%p))\n", win);
  wait_for_return();
}

void glk_window_close(winid_t win, stream_result_t *result)
{
  printf("(glk-window-close (glki-pointer '%p))\n", win);
  wait_for_return();
}

void glk_window_clear(winid_t win)
{
  printf("(glk-window-clear (glki-pointer '%p))\n", win);
  wait_for_return();
}

void glk_window_get_size(winid_t win, glui32 *widthptr, glui32 *heightptr)
{
  printf("(glk-window-get-size (glki-pointer '%p))\n", win);
  *widthptr = get_next_glui32();
  *heightptr = get_next_glui32();
}

void glk_window_move_cursor(winid_t win, glui32 xpos, glui32 ypos)
{
  printf("(glk-window-move-cursor (glki-pointer '%p) (glki-glui32 %d) (glki-glui32 %d))\n", win, xpos, ypos);
  wait_for_return();
}
