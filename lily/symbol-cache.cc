/*   
  symbol-cache.cc --  implement a cache for literal symbols, eg
    symbol("foo-bar")
    
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include <map>
#include "lily-guile.hh" 

#if 0

typedef map<const char*, SCM> Literal_symbol_map;
Literal_symbol_map literal_map;


SCM
symbol (const char*s)
{
  Literal_symbol_map::const_iterator i = literal_map.find (s);
  if (i != literal_map.end())
    return (*i).second;

  SCM sym = gh_symbol2scm ((char*)s);
  scm_permanent_object (sym);
  literal_map[s] = sym;
  return sym;
}


/*
  This is a gory trick to cache the value gh_symbol2scm (), without
  cluttering up the C code with snarf macros.
  
  You should *ONLY* use symbol() for arguments that are literal
  strings!
  
  without (wtk1-fugue2)

	real	0m20.157s
	user	0m19.800s
	sys	0m0.140s

  with: (per file.)

	real	0m19.284s
	user	0m18.630s
	sys	0m0.190s


  global with STL map

	real	0m20.616s
	user	0m19.360s
	sys	0m0.080s

  global with binsearch.

	real	0m19.352s
	user	0m18.710s
	sys	0m0.230s

  local binsearch

   user 18.8

  local with binsearch, and other optimizations.

   17.7
*/
#endif


