/*
  timing-grav.cc -- implement Timing_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "translator-group.hh"
#include "command-request.hh"
#include "grob-info.hh"
#include "multi-measure-rest.hh"
#include "timing-translator.hh"
#include "engraver.hh"

/**
  Do time bookkeeping
 */
class Timing_engraver : public Timing_translator, public Engraver
{   
protected:
  virtual void start_translation_timestep ();
  virtual void stop_translation_timestep ();
  virtual void process_music ();
public:
  VIRTUAL_COPY_CONS(Translator);
};

ADD_THIS_TRANSLATOR(Timing_engraver);

void
Timing_engraver::start_translation_timestep( )
{
  Timing_translator::start_translation_timestep ();

  SCM nonauto = get_property ("barNonAuto");

  SCM which = get_property ("whichBar");
  if (!gh_string_p (which))
    which = now_mom ()
      ? SCM_EOL : ly_str02scm ("|");
  
  if (!gh_string_p (which) && !to_boolean (nonauto))
    {
      SCM always = get_property ("barAlways");
      if (!measure_position ()
	  || (to_boolean (always)))
	{
	  /* should this work, or be junked?  See input/bugs/no-bars.ly */
	  which=get_property ("defaultBarType" );
	}
    }

  daddy_trans_l_->set_property ("whichBar", which);
}

void
Timing_engraver::stop_translation_timestep ()
{
  Timing_translator::stop_translation_timestep ();
  daddy_trans_l_->set_property ("whichBar", SCM_EOL);  
}


/*
  ugh. Translator doesn't do process_music ().
 */
void
Timing_engraver::process_music ()
{
  Timing_translator::process_music ();
}
