/*
  misc.cc -- implement various stuff

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/


#include "misc.hh"
#include "string.hh"

/*
  Return the 2-log, rounded down
*/
int
intlog2 (int d)
{
  assert (d);
  int i = 0;
  while ((d != 1))
    {
      d /= 2;
      i++;
    }

  assert (! (d / 2));
  return i;
}

double
log_2 (double x)
{
  return log (x) / log (2.0);
}

Array<String>
split_string (String s, char c)
{
  Array<String> rv;
  while (s.length ())
    {
      int i = s.index (c);

      if (i == 0)
	{
	  s = s.nomid_string (0, 1);
	  continue;
	}

      if (i < 0)
	i = s.length ();

      rv.push (s.cut_string (0, i));
      s = s.nomid_string (0, i);
    }

  return rv;
}


Real
directed_round (Real f, Direction d)
{
  if (d < 0)
    return floor (f);
  else
    return ceil (f);
}

