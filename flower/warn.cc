#include <stdlib.h>
#include <stdio.h>

#include "warn.hh"


void
error (String s)
{
  fputs ( _f("error: %s\n", s.ch_C()).ch_C(), stderr);  
  exit (1);
}

void
non_fatal_error (String s)
{
  fputs ( _f("error: %s\n", s.ch_C()).ch_C(), stderr);  
}

void
warning (String m)
{
  fputs ( _f("warning: %s\n", m.ch_C()).ch_C(), stderr);
}

void
message (String m)
{
  fprintf (stderr, "%s\n",m.ch_C());
}

void
programming_error (String s)
{
  fputs ( _f("programming error: %s (Continuing; cross thumbs)\n", s.ch_C()).ch_C(), stderr);  
}

