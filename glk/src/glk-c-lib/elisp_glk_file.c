#include <stdio.h>
#include <stdlib.h>
#include "glk.h"
#include "elisp_glk.h"

frefid_t glk_fileref_create_by_name(glui32 usage, char *name, glui32 rock)
{
  frefid_t file_id = malloc(sizeof(fileref_t));

  char *escaped = escape_backslashes(name);


  printf("(glk-fileref-create-by-name (glki-glui32 %d) (glki-string \"%s\") (glki-glui32 %d) (glki-pointer '%p))\n",
         usage, escaped, rock, file_id);

  free(escaped);
  wait_for_return();
  return file_id;
}

frefid_t glk_fileref_create_by_prompt(glui32 usage, glui32 fmode, glui32 rock)
{
  frefid_t file_id = malloc(sizeof(fileref_t));

  printf("(glk-fileref-create-by-prompt (glki-glui32 %d) (glki-glui32 %d) (glki-glui32 %d) (glki-pointer '%p))\n",
         usage, fmode, rock, file_id);

  wait_for_return();
  return file_id;
}

void glk_fileref_destroy(frefid_t fref)
{
  printf("(glk-fileref-destroy (glki-pointer '%p))\n", fref);
  wait_for_return();
}

