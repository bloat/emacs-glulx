#include <stdio.h>
#include <stdlib.h>

#include "glk.h"
#include "elisp_glk.h"

int main(int argc, char *argv[])
{
  strid_t game_file = malloc(sizeof(stream_t));

  printf("(glki-set-game-file-stream (glki-pointer '%p))\n", game_file);
  wait_for_return();

  glk_elisp_set_game_file_stream(game_file);
  glk_main();

  free(game_file);
  return 0;
}
