#include <stream.h>
#include "warn.hh"


void
error (String s)
{
  cerr <<  _ ("error: ") << s << '\n';

  exit (1);
}

void
non_fatal_error (String s)
{
  cerr <<  _ ("error: ") << s << '\n';
}

void
warning (String m)
{
  cerr << _ ("warning: ") <<m <<endl;

}

void
message (String m)
{
  cerr << m<<endl;
}

void
programming_error (String s)
{
  cerr << _("programming error: ") << s << _(" (Continuing; cross thumbs)") << '\n';
}

