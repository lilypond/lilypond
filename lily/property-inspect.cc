/*   
  property-inspect.cc --  implement Property inspect funcs.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "property-inspect.hh"

bool
isdir_b (SCM s)
{
  if (gh_number_p (s))
    {
      int i = gh_int2scm (s);
      return i>= -1 && i <= 1; 
    }
  return false;
}

Direction
to_dir (SCM s)
{
  return (Direction) gh_scm2int (s);
}
