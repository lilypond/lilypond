/*
  bar-check-iterator.cc -- implement Bar_check_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2001--2009  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "context.hh"
#include "input.hh"
#include "international.hh"
#include "music.hh"
#include "simple-music-iterator.hh"

/*
  Check bar checks. We do this outside the engravers so that you can
  race through the score using skipTypesetting to correct durations.
*/
class Bar_check_iterator : Simple_music_iterator
{
public:
  virtual void process (Moment);
  Bar_check_iterator ();
  DECLARE_SCHEME_CALLBACK (constructor, ());
};

IMPLEMENT_CTOR_CALLBACK (Bar_check_iterator);

Bar_check_iterator::Bar_check_iterator ()
{
}

void
Bar_check_iterator::process (Moment m)
{
  Simple_music_iterator::process (m);
  if (!m.to_bool ())
    {
      Context *tr = get_outlet ();

      SCM check = tr->get_property ("ignoreBarChecks");
      if (to_boolean (check))
	return;

      SCM mp = tr->get_property ("measurePosition");
      SCM sync = tr->get_property ("barCheckSynchronize");

      Moment *where = unsmob_moment (mp);
      if (!where)
	return;

      if (where->main_part_)
	{
	  bool warn = true;
	  if (to_boolean (sync))
	    {
	      SCM mp;
	      tr = tr->where_defined (ly_symbol2scm ("measurePosition"), &mp);
	      Moment zero;
	      tr->set_property ("measurePosition", zero.smobbed_copy ());
	    }
	  else
	    {
	      SCM lf = tr->get_property ("barCheckLastFail");
	      if (unsmob_moment (lf)
		  && *unsmob_moment (lf) == *where)
		warn = false;
	      else
		tr->set_property ("barCheckLastFail", mp);
	    }

	  if (warn)
	    get_music ()->origin ()->warning (_f ("barcheck failed at: %s",
						  where->to_string ()));
	}
    }
}
