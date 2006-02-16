/*
  misc.cc -- implement various stuff

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/


#include "misc.hh"
#include "std-string.hh"

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

vector<string>
split_string (string str, char c)
{
  vector<string> a;
  ssize i = str.find (c);
  while (i != NPOS)
    {
      string s = str.substr (0, i);
      a.push_back (s);
      while (str[++i] == c)
	;
      str = str.substr (i);
      i = str.find (c);
    }
  if (str.length ())
    a.push_back (str);
  return a;
}

#if 0
vector<string>
split_string (string s, char c)
{
  vector<string> rv;
  while (s.length ())
    {
      ssize i = s.find (c);

      if (i == 0)
	{
	  s = s.substr (1, s.length () -1);
	  continue;
	}

      if (i == NPOS)
	i = s.length ();

      rv.push_back (s.substr (0, i));
      s = s.substr (i, s.length () - i);
    }

  return rv;
}
#endif


Real
directed_round (Real f, Direction d)
{
  if (d < 0)
    return floor (f);
  else
    return ceil (f);
}

