/*
  bar-engraver.cc -- implement Bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "bar-line.hh"
#include "score-engraver.hh"
#include "event.hh"
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
  TRANSLATOR_DECLARATIONS(  Bar_engraver );
  void request_bar (String type_string);
    
protected:
  virtual void finalize ();
  virtual void stop_translation_timestep ();
  virtual void process_acknowledged_grobs ();

private:
  void typeset_bar ();
  void create_bar ();

  Item * bar_;
};

Bar_engraver::Bar_engraver ()
{
  bar_ =0;
}

void
Bar_engraver::create_bar ()
{
  if (!bar_)
    {
      bar_ = new Item (get_property ("BarLine"));
      SCM gl = get_property ("whichBar");
      if (scm_equal_p (gl, bar_->get_grob_property ("glyph")) != SCM_BOOL_T)
	  bar_->set_grob_property ("glyph", gl);
      
      announce_grob(bar_, SCM_EOL);
    }
}

void
Bar_engraver::finalize ()
{
  typeset_bar ();
}

/*
  Bar_engraver should come *after* any engravers that  
  modify whichBar

  This is a little hairy : whichBar may be set by
  Repeat_acknowledge_engraver::process_music, which is at score
  context. This means that grobs could should be created after
  process_music. We do stuff process_acknowledged_grobs(), just to be
  on the safe side.
     
*/

void
Bar_engraver::process_acknowledged_grobs ()
{
  if (!bar_ && gh_string_p (get_property ("whichBar")))
    {
      create_bar ();
    }
}

void
Bar_engraver::typeset_bar ()
{
  if (bar_) 
    {
      typeset_grob (bar_);
      bar_ =0;
    }
}

/*
  lines may only be broken if there is a barline in all staves 
*/
void 
Bar_engraver::stop_translation_timestep ()
{
  if (!bar_)
    {
      top_engraver ()->forbid_breaks ();	// guh. Use properties!
    }
  else
    typeset_bar ();
}


ENTER_DESCRIPTION(Bar_engraver,
/* descr */       "Create barlines. This engraver is controlled through the
@code{whichBar} property. If it has no bar line to create, it will forbid a linebreak at this point",
/* creats*/       "BarLine",
/* accepts */     "",
/* acks  */      "",
/* reads */       "whichBar",
/* write */       "");
