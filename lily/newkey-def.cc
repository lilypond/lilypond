/*   
  newkey-def.cc --  implement Newkey_def
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "newkey-def.hh"
#include "misc.hh"


Newkey_def::Newkey_def()
{
  pitch_alist_ = SCM_EOL;
}

void
Newkey_def::transpose (Musical_pitch p) 
{
  SCM newlist = SCM_EOL;
  for (SCM s = pitch_alist_; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM k = gh_caar (s);

      if (gh_pair_p (k))
	{
	  Musical_pitch orig (gh_list (gh_car (k), gh_cdr (k), gh_cdr (s), SCM_UNDEFINED));

	  orig.transpose (p);

	  SCM key = gh_cons (gh_int2scm (orig.octave_i_),
			     gh_int2scm (orig.notename_i_));

	  newlist = gh_cons (gh_cons (key, gh_int2scm (orig.accidental_i_)),
			     newlist);
	}
      else if (gh_number_p (k))
	{
	  Musical_pitch orig (gh_list (gh_int2scm (0), k, gh_cdar (s), SCM_UNDEFINED));
	  orig.transpose (p);

	  SCM key =gh_int2scm (orig.notename_i_);
	  newlist = gh_cons (gh_cons (key, gh_int2scm (orig.accidental_i_)),
			     newlist);
	}
    }

  pitch_alist_ = newlist;
}
