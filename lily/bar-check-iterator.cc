/*   

     bar-check-iterator.cc -- implement Bar_check_iterator

     source file of the GNU LilyPond music typesetter

     (c) 2001--2002  Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include "simple-music-iterator.hh"
#include "event.hh"
#include "translator-group.hh"

/*
  Check bar checks. We do this outside the engravers so that you can
  race through the score using skipTypesetting to correct durations.
 */
class Bar_check_iterator : Simple_music_iterator
{
public:
  VIRTUAL_COPY_CONS(Bar_check_iterator);
  virtual void process (Moment);
  Bar_check_iterator( );
  DECLARE_SCHEME_CALLBACK(constructor, ());
};

IMPLEMENT_CTOR_CALLBACK(Bar_check_iterator);

Bar_check_iterator::Bar_check_iterator()
{
}

void
Bar_check_iterator::process (Moment m)
{
  Simple_music_iterator::process(m);
  if (!m.to_bool ())
    {
      Translator_group *tr = report_to ();

      SCM mp = tr->get_property ("measurePosition");
      SCM sync= tr->get_property ("barCheckSynchronize");

      Moment * where =unsmob_moment (mp);
      if (!where)
	return;
      
      if (where->main_part_)
	{
	  get_music ()->origin ()->warning (_f ("barcheck failed at: %s", 
					      where->string ()));
	  if (to_boolean (sync))
	    {
	      tr = tr->where_defined (ly_symbol2scm("measurePosition"));
	      Moment zero;
	      tr->set_property ("measurePosition", zero.smobbed_copy ());
	    }
	}
    }
}    
