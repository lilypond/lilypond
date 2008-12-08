/*
  unfolded-repeat-iterator.cc -- implement Unfolded_repeat_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2002--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "music.hh"
#include "sequential-iterator.hh"
#include "context.hh"

class Unfolded_repeat_iterator : public Sequential_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
protected:
  virtual SCM get_music_list () const;
};

SCM
Unfolded_repeat_iterator::get_music_list () const
{
  SCM l = SCM_EOL;
  SCM *tail = &l;

  SCM body = get_music ()->get_property ("element");
  SCM alts = get_music ()->get_property ("elements");
  int alt_count = scm_ilength (alts);
  int rep_count = scm_to_int (get_music ()->get_property ("repeat-count"));

  for (int i = 0; i < rep_count; i++)
    {
      if (unsmob_music (body))
	*tail = scm_cons (body, SCM_EOL);

      tail = SCM_CDRLOC (*tail);

      if (alt_count)
	{
	  *tail = scm_cons (scm_car (alts), SCM_EOL);
	  tail = SCM_CDRLOC (*tail);
	  if (i >= rep_count - alt_count)

	    alts = scm_cdr (alts);
	}
    }

  return l;
}

IMPLEMENT_CTOR_CALLBACK (Unfolded_repeat_iterator);
