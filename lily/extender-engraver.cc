/*
  extender-engraver.cc -- implement Extender_engraver

  source file of the GNU LilyPond music typesetter
  
  (c)  1999--2003 Glen Prideaux <glenprideaux@iname.com>,
                  Han-Wen Nienhuys <hanwen@cs.uu.nl>,
                  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "warn.hh"
#include "lyric-extender.hh"
#include "item.hh"
#include "engraver.hh"

class Extender_engraver : public Engraver
{
  Music* ev_;
  Spanner* extender_;
  Spanner * finished_extender_;  
public:
  TRANSLATOR_DECLARATIONS(Extender_engraver);

protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void finalize ();
  virtual bool try_music (Music*);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void process_music ();
private:

};




Extender_engraver::Extender_engraver ()
{
  extender_ = 0;
  finished_extender_ = 0;
  ev_ = 0;
}

void
Extender_engraver::acknowledge_grob (Grob_info i)
{
  Item * item =  dynamic_cast<Item*> (i.grob_);
  // -> text_item
  if (item && item->internal_has_interface (ly_symbol2scm ("lyric-syllable-interface")))
    {
      if (extender_)
	extender_->set_bound (LEFT, item);

      if (finished_extender_)
	finished_extender_->set_bound (RIGHT, item);
    }
}


bool
Extender_engraver::try_music (Music* r)
{
  if (ev_)
    return false;

  ev_ = r;
  return true;
}

void
Extender_engraver::finalize ()
{
  if (extender_)
    {
      extender_->warning (_ ("unterminated extender"));
      typeset_grob (extender_);
      extender_ = 0;
    }

  if (finished_extender_)
    {
      finished_extender_->warning (_("unterminated extender"));
      typeset_grob (finished_extender_);
      finished_extender_ =0;
    }
}

void
Extender_engraver::process_music ()
{
  if (ev_)
    {
      extender_ = new Spanner (get_property ("LyricExtender"));
      announce_grob (extender_, ev_->self_scm());
    }
}


void
Extender_engraver::stop_translation_timestep ()
{
  if (finished_extender_ && finished_extender_->get_bound (RIGHT))
    {
      typeset_grob (finished_extender_);
      finished_extender_ = 0;
    }

  if (finished_extender_ && extender_)
    {
      programming_error ("Haven't finished extender yet.");
      typeset_grob (finished_extender_);
      finished_extender_ =0;
    }
  
  if (extender_)
    finished_extender_ = extender_;
  extender_ = 0;
}

void
Extender_engraver::start_translation_timestep ()
{
  ev_ = 0;
}


ENTER_DESCRIPTION(Extender_engraver,
/* descr */       "Create lyric extenders",
/* creats*/       "LyricExtender",
/* accepts */     "extender-event",
/* acks  */       "lyric-syllable-interface",
/* reads */       "",
/* write */       "");
