#include <fstream.h>
#include "flower-debug.hh"
#include "dstream.hh"

bool flower_check_debug=false;
ofstream null_device ("/dev/null");
Dstream default_flower_stream (&null_device ,"/dev/null");
Dstream *flower_dstream  = &default_flower_stream;

/**
  Set the debugging output. Will not delete/swallow argument.
 */
void set_flower_debug (Dstream&ds, bool b)
{
#ifdef NPRINT
  if (b)
    cout << _ ("Debug output disabled.  Compiled with NPRINT.") << endl;
#endif

  flower_check_debug = b;
  flower_dstream = &ds;
}
