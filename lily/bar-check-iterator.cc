/*   
     bar-check-iterator.cc -- implement Bar_check_iterator

     source file of the GNU LilyPond music typesetter

     (c) 2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include "simple-music-iterator.hh"
#include "command-request.hh"
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
  static SCM constructor_cxx_function;
};

IMPLEMENT_CTOR_CALLBACK (Bar_check_iterator);

Music * get_barcheck ()
{
  Music *bc = new Music;
  bc->set_mus_property ("iterator-ctor", Bar_check_iterator::constructor_cxx_function);
  return bc;
}

Bar_check_iterator::Bar_check_iterator()
{
}

void
Bar_check_iterator::process (Moment m)
{
  if (m == Moment (0))
    {
      Translator_group *tr = report_to_l ();

      SCM mp = tr->get_property (ly_symbol2scm ("measurePosition"));
      SCM sync= tr->get_property (ly_symbol2scm ("barCheckNoSynchronize"));

      Moment * where =unsmob_moment (mp);
      if (where && *where) 
	{
	  music_l_->origin ()->warning (_f ("barcheck failed at: %s", 
					    where->str ()));

      
	  if (!to_boolean (sync))
	    {
	      tr = tr->where_defined (ly_symbol2scm("measurePosition"));
	      Moment zero;
	      tr->set_property (ly_symbol2scm ("measurePosition"), zero.smobbed_copy ());
	    }
	}
    }
  Simple_music_iterator::process(m);

}
    
