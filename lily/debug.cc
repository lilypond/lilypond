/*   
  debug.cc --  implement debugging routines
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1996,98 Han-Wen Nienhuys <hanwen@stack.nl>
  
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
#include "main.hh"
Dstream *monitor=0;
ostream * nulldev =0;


// ugh
struct _Dinit {
  _Dinit()
    {
	nulldev = new ofstream ("/dev/null");
	monitor = new Dstream (&cout,".dstreamrc");
    }
  ~_Dinit()
    {
	delete nulldev;
	delete monitor;
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
  cerr << _("Floating point exception .. \n")<< flush;
  assert (false);
}

/// just to make sure print_rat is linked in
static void (*rat_printer)(Moment const&);

void
debug_init()
{
  rat_printer = print_rat;
#ifndef NDEBUG
  // libg++ 2.8.0 doesn't have set_new_handler
  // set_new_handler (&mynewhandler);
#endif
  set_flower_debug (*monitor, check_debug);

  signal (SIGFPE, float_handler);
}

bool check_debug=false;


bool check_malloc_b = false;

// #define MEMORY_PARANOID

#ifdef MEMORY_PARANOID

#include <malloc.h>

void *
operator new (size_t size)
{
  void *result;
  result = malloc (size);
  if (check_malloc_b)
    memfrob (result, size);
  return result;
}


void 
operator delete (void *p)
{
  if (!p)
    return ;
  if (check_malloc_b)
    memfrob (p, 8);		// ugh.  Need the blocksize.   8 is sysdependant

  free (p);
}
#endif // MEMORY_PARANOID

void
set_debug (bool b)
{
  check_debug =b;
  set_flower_debug (*monitor, check_debug);
  check_malloc_b = experimental_features_global_b;
#ifdef MEMORY_PARANOID
  if (check_malloc_b)
    if (mcheck (0))
      warning ("Can't set mem-checking!");
#endif
}

