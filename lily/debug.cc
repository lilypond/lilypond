/*   
  debug.cc --  implement debugging routines
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1996,98 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include <fstream.h>
#include <signal.h>

// libg++ 2.8.0
// #include <std/new.h>
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

// #define MEMORY_PARANOID

#ifdef MEMORY_PARANOID

#include <malloc.h>

void
frobnify (void *p, size_t s)
{
  char *cp = (char*)p;
  char *e  = cp + s;
  while (cp < e)
    {
      *cp++ ^=42;
    }
}


void *
operator new (size_t size)
{
  void *result;
  result = malloc (size);
  if (check_malloc_b)
    frobnify (result, size);
  return result;
}

void *to_frob; int frob_size;

void
set_frobnify (void * p, size_t sz)
{
  to_frob = p;
  frob_size = sz;
}

void 
operator delete (void *p)
{
  if (!p)
    return ;
  if (p == to_frob)
    {
      frobnify (p, frob_size);
      to_frob = 0;
      frob_size=0;
    }

  free (p);
}
#endif // MEMORY_PARANOID

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
      warning (_ ("Can't set mem-checking!"));
#endif
}

