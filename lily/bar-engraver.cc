/*
  bar-engraver.cc -- implement Bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997, 1998, 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "score-engraver.hh"
#include "bar-engraver.hh"
#include "staff-bar.hh"
#include "musical-request.hh"
#include "multi-measure-rest.hh"
#include "command-request.hh"
#include "timing-engraver.hh"
#include "engraver-group-engraver.hh"
#include "warn.hh"

Bar_engraver::Bar_engraver()
{
  bar_p_ =0;
  do_post_move_processing();
}




void
Bar_engraver::create_bar ()
{
  if (!bar_p_)
    {
      bar_p_ = new Staff_bar;
      bar_p_->set_elt_property ("break-aligned", SCM_BOOL_T);

      // urg: "" != empty...
      SCM default_type = get_property ("defaultBarType");
      if (gh_string_p (default_type))
	{
	  bar_p_->set_elt_property ("glyph", default_type); // gu.h
	}

#if 0
      /*
	urg.  Why did I implement this? And did I implement this so
	clumsily?

	input/test/just-friends.ly
	Maybe a staffgroup of just one staff would be a better solution.
      */
      SCM prop = get_property ("barAtLineStart");
      if (to_boolean (prop))
	{
	  bar_p_->set_elt_property ("at-line-start", SCM_BOOL_T);
	}
#endif
      announce_element (Score_element_info (bar_p_, 0));
    }
}

/**
   Make a barline.  If there are both |: and :| requested, merge them
   to :|:.

*/
void
Bar_engraver::request_bar (String requested_type)
{
  if (!now_mom ())
    {
      SCM prop = get_property ("barAtLineStart");
      if (!to_boolean (prop))
	return;
    }
  bool  bar_existed = bar_p_;
  create_bar ();
  if (bar_existed && requested_type == "")
    {
      return;
    }

  String current = ly_scm2string (bar_p_->get_elt_property ("glyph"));
  
  if ((requested_type == "|:" && current== ":|")
    || (requested_type == ":|" && current == "|:"))
    requested_type = ":|:";

  
  bar_p_->set_elt_property ("glyph",
			    ly_str02scm (requested_type.ch_C ()));
}

void 
Bar_engraver::do_creation_processing ()
{
}

void
Bar_engraver::do_removal_processing ()
{
  if (bar_p_) 
    {
      typeset_element (bar_p_);
      bar_p_ =0;
    }
}

void
Bar_engraver::do_process_music()
{  
  Translator * t = daddy_grav_l  ()->get_simple_translator ("Timing_engraver");

  Timing_engraver * te = dynamic_cast<Timing_engraver*>(t);
  String which = (te) ? te->which_bar () : "";

  if (which.length_i ())
    {
      create_bar();
      bar_p_->set_elt_property ("glyph",  ly_str02scm (which.ch_C ()));
    }
  
  if (!bar_p_)
    {
      Score_engraver * e = 0;
      Translator * t  =  daddy_grav_l ();
      for (; !e && t;  t = t->daddy_trans_l_)
	{
	  e = dynamic_cast<Score_engraver*> (t);
	}
      
      if (!e)
	programming_error ("No score engraver!");
      else
	e->forbid_breaks ();
    }
}


void 
Bar_engraver::do_pre_move_processing()
{
  if (bar_p_) 
    {
      typeset_element (bar_p_);
      bar_p_ =0;
    }
}

ADD_THIS_TRANSLATOR(Bar_engraver);



