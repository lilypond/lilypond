/*
  bar-engraver.cc -- implement Bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "bar.hh"
#include "score-engraver.hh"
#include "bar-engraver.hh"
#include "musical-request.hh"
#include "multi-measure-rest.hh"
#include "command-request.hh"

#include "engraver-group-engraver.hh"
#include "warn.hh"
#include "item.hh"

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
      bar_p_ = new Item (get_property ("basicBarProperties"));
      announce_element (bar_p_, 0);
    }
}

void 
Bar_engraver::do_creation_processing ()
{
}

void
Bar_engraver::do_removal_processing ()
{
  typeset_bar ();
}

/*
  Bar_engraver should come *after* any engravers that expect bars to
  modify whichBar in do_process_music () be typeset
*/
void
Bar_engraver::do_process_music()
{
  SCM b =get_property ("whichBar");
  if (gh_string_p (b))
    {
      create_bar ();
    }
}

void
Bar_engraver::typeset_bar ()
{
  if (bar_p_) 
    {
      SCM gl = get_property ("whichBar");
      if (scm_equal_p (gl, bar_p_->get_elt_property ("glyph")) != SCM_BOOL_T)
	  bar_p_->set_elt_property ("glyph", gl);
      typeset_element (bar_p_);
      bar_p_ =0;
    }
}

/*
  lines may only be broken if there is a barline in all staffs 
*/
void 
Bar_engraver::do_pre_move_processing()
{
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
	e->forbid_breaks ();	// guh. Use properties!
    }
  else
    typeset_bar ();
}

ADD_THIS_TRANSLATOR(Bar_engraver);



