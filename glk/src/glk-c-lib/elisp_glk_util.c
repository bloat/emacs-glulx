#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* escape_backslashes(char* string)
{
     int i = 0;
     int j = 0;
     int bs_count = 0;
     int new_length;
     char* new_string;

     while (string[i] != '\0')
     {
          if (string[i] == '\\')
          {
               bs_count++;
          }
          if (string[i] == '\"')
          {
               bs_count++;
          }
          i = i + 1;
     }

     new_length = strlen(string) + bs_count + 1;
     new_string = malloc(new_length);

     i = 0;

     while (string[i] != '\0')
     {
          if (string[i] == '\\' || string[i] == '\"')
          {
               new_string[j++] = '\\';
          }
          new_string[j++] = string[i++];
     }
     new_string[j] = '\0';

     return new_string;
}
