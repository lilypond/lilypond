/*
  timing-grav.cc -- implement Timing_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "score-engraver.hh"
#include "timing-engraver.hh"
#include "command-request.hh"
#include "score-element-info.hh"
#include "multi-measure-rest.hh"



ADD_THIS_TRANSLATOR(Timing_engraver);


void
Timing_engraver::do_post_move_processing( )
{
  bar_req_l_ = 0;
  Timing_translator::do_post_move_processing ();
}

bool
Timing_engraver::do_try_music (Music*m)
{
  if (Bar_req  * b= dynamic_cast <Bar_req *> (m))
    {
      if (bar_req_l_ && bar_req_l_->equal_b (b)) // huh?
	return false;
      
      bar_req_l_ = b;
      return true;
    }
  
  return Timing_translator::do_try_music (m);
}


String
Timing_engraver::which_bar ()
{
  if (!bar_req_l_)
    {
      if (!now_mom ())
	return "|";

      SCM nonauto = get_property ("barNonAuto");
      if (!to_boolean (nonauto))
	{
	  SCM always = get_property ("barAlways");
	  if (!measure_position ()
	      || (to_boolean (always)))
	    {
	      SCM def=get_property ("defaultBarType" );
	      return (gh_string_p (def))? ly_scm2string (def) : "";
	    }
	}
      return "";
    }
  else
    {
      return bar_req_l_->type_str_;
    }
}
