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

Dstream *lily_monitor=0;
ostream * nulldev =0;


// ugh
struct _Dinit {
  _Dinit()
    {
	nulldev = new ofstream ("/dev/null");
	lily_monitor = new Dstream (&cout,".dstreamrc");
    }
  ~_Dinit()
    {
	delete nulldev;
	delete lily_monitor;
    }
} dinit;



/*
  want to do a stacktrace .
  */
void
mynewhandler()
{
  assert (false);
}

void
float_handler (int)
{
  cerr << _ ("Floating point exception") << endl;
  assert (false);
}


void
debug_init()
{
#ifndef NDEBUG
  // libg++ 2.8.0 doesn't have set_new_handler
  // set_new_handler (&mynewhandler);
#endif
  set_flower_debug (*lily_monitor, check_debug);

  signal (SIGFPE, float_handler);
}

bool check_debug=false;


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
  check_debug =b;
  set_flower_debug (*lily_monitor, check_debug);
#ifdef MEMORY_PARANOID
  if (check_malloc_b)
    if (mcheck (0))
      warning (_ ("can't set mem-checking") + "!");
#endif
}

