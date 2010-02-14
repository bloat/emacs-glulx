#ifndef _ELISP_GLK_H
#define _ELISP_GLK_H

#include "glk.h"

extern void wait_for_return();
extern glui32 get_next_glui32();
extern void* get_next_pointer();
extern void get_next_string(char* buffer);

extern char* escape_backslashes(char* string);

extern void glk_elisp_set_game_file_stream(strid_t game_file);

typedef struct glk_window_struct {
} window_t;

typedef struct glk_stream_struct {
} stream_t;

typedef struct glk_fileref_struct {
} fileref_t;

#endif /* _ELISP_GLK_H */
