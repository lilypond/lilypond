/*
  bar-engraver.cc -- implement Bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "bar.hh"
#include "score-engraver.hh"
#include "musical-request.hh"
#include "engraver-group-engraver.hh"
#include "warn.hh"
#include "item.hh"
#include "engraver.hh"

/*
  generate bars. Either user ("|:"), or default (new measure)

  */
class Bar_engraver : public Engraver
{
public:
  Bar_engraver();
  VIRTUAL_COPY_CONS(Translator);
  void request_bar (String type_str);
    
protected:
  virtual void finalize ();
  virtual void stop_translation_timestep();
  virtual void create_grobs ();

private:
  void typeset_bar ();
  void create_bar ();

  Item * bar_p_;
};

Bar_engraver::Bar_engraver()
{
  bar_p_ =0;
}

void
Bar_engraver::create_bar ()
{
  if (!bar_p_)
    {
      bar_p_ = new Item (get_property ("BarLine"));

      SCM gl = get_property ("whichBar");
      if (scm_equal_p (gl, bar_p_->get_grob_property ("glyph")) != SCM_BOOL_T)
	  bar_p_->set_grob_property ("glyph", gl);
      
      announce_grob (bar_p_, 0);
    }
}

void
Bar_engraver::finalize ()
{
  typeset_bar ();
}

/*
  Bar_engraver should come *after* any engravers that expect bars to
  modify whichBar in  deprecated_process_music () be typeset
*/
void
Bar_engraver::create_grobs ()
{
  if (!bar_p_ && gh_string_p (get_property ("whichBar")))
    {
      create_bar ();
    }
}

void
Bar_engraver::typeset_bar ()
{
  if (bar_p_) 
    {
      typeset_grob (bar_p_);
      bar_p_ =0;
    }
}

/*
  lines may only be broken if there is a barline in all staffs 
*/
void 
Bar_engraver::stop_translation_timestep()
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
