#ifdef TEST_TTFTOOL

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "ttftool.h"


main (int argc, char **argv)
{
  FILE *in, *out;
  if (argc != 3)
    {
      fprintf (stderr, "test foo.ttf bar.pfa");
      exit (2);
    }

  out = fopen (argv[2], "w");
  assert(out);
  create_type42 (argv[1], (void*) out);
}

#endif
