#include <stdio.h>
#include <glk.h>
#include <elisp_glk.h>
#include <string.h>

strid_t game_file_stream;

void glk_elisp_set_game_file_stream(strid_t game_file)
{
  game_file_stream = game_file;
}

void glk_main(void)
{
  event_t ev;
  char buf[256];
  char hello[7];
  glui32 width;
  glui32 height;
  frefid_t fref;
  strid_t stream;

  /* Open the main window. */
  winid_t mainwin = glk_window_open(0, 0, 0, wintype_TextBuffer, 0);
  if (mainwin == NULL)
  {
    return;
  }

  glk_set_window(mainwin);

  glk_put_string("Hello, Elisp GLK!");

  fref = glk_fileref_create_by_name(0, "/home/acowper/bosh.txt", 0);
  stream = glk_stream_open_file(fref, 0, 0);

  glk_get_buffer_stream(stream, buf, 16);

  glk_put_string(buf);
}
