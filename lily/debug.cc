/*   
  debug.cc --  implement debugging routines
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include <fstream.h>
#include <signal.h>
#include <stdlib.h>

#include "debug.hh"
#include "dstream.hh"
#include "flower-debug.hh"
#include "moment.hh"
#include "misc.hh"
#include "main.hh"

Dstream *my_monitor=0;

void
float_handler (int)
{
  cerr << _ ("floating point exception") << endl;
  assert (false);
}

void
debug_init()
{
  my_monitor = new Dstream (&cout, ".dstreamrc");
  signal (SIGFPE, float_handler);
}

bool check_malloc_b = false;

void
set_debug (bool b)
{
  if (b)
    flower_dstream = my_monitor;
  else
    flower_dstream = 0;
  
#ifdef MEMORY_PARANOID
  if (check_malloc_b)
    if (mcheck (0))
      warning (_ ("can't set mem-checking!"));
#endif
}

