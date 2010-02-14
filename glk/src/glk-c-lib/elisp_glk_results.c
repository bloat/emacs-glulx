#include <stdio.h>
#include <stdlib.h>

#include "glk.h"
#include "elisp_glk.h"

int get_value_length()
{
  char length_buf[4];
  fgets(length_buf, 4, stdin);
  return atoi(length_buf);
}

glui32 get_next_glui32()
{
  int length = get_value_length();
  // Read the length of the value, the newline and fgets will append the \0
  char value_buf[length + 2];
  fgets(value_buf, length + 2, stdin);
  return strtoul(value_buf, NULL, 10);
}

glsi32 get_next_glsi32()
{
  return get_next_glui32();
}

void *get_next_pointer()
{
  void *pointer;
  int length = get_value_length();
  char value_buf[length + 2];
  fgets(value_buf, length + 2, stdin);

  if (strcmp(value_buf, "NULL\n") == 0)
  {
    return NULL;
  }

  sscanf(value_buf, "%p", &pointer);
  return pointer;
}

void get_next_string(char* buffer)
{
  int length = get_value_length();
  char* some_buffer = malloc(length + 2);
  fgets(buffer == NULL ? some_buffer : buffer, length + 2, stdin);
  free(some_buffer);
}

void wait_for_return()
{
  (void) fgetc(stdin);
}

unsigned char get_next_char()
{
  int length = get_value_length();
  char value_buf[length + 2];
  fgets(value_buf, length + 2, stdin);
  return (unsigned char) atoi(value_buf);
}
