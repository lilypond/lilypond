/*
  extender-engraver.cc -- implement Extender_engraver

  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Glen Prideaux <glenprideaux@iname.com>,
                  Han-Wen Nienhuys <hanwen@cs.uu.nl>,
                  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "context.hh"
#include "engraver.hh"
#include "group-interface.hh"
#include "item.hh"
#include "lyric-extender.hh"
#include "note-head.hh"
#include "warn.hh"

void completize_extender (Spanner *sp);

class Extender_engraver : public Engraver
{
  Music *ev_;
  Spanner *extender_;
  Spanner *pending_extender_;

public:
  TRANSLATOR_DECLARATIONS (Extender_engraver);

protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void finalize ();
  virtual bool try_music (Music*);
  virtual void stop_translation_timestep ();
  virtual void process_music ();
};


Extender_engraver::Extender_engraver ()
{
  extender_ = 0;
  pending_extender_ = 0;
  ev_ = 0;
}

bool
Extender_engraver::try_music (Music *r)
{
  if (!ev_)
    {
      ev_ = r;
      return true;
    }
  return false;
}

void
Extender_engraver::process_music ()
{
  if (ev_)
    extender_ = make_spanner ("LyricExtender", ev_->self_scm ());
}

void
Extender_engraver::acknowledge_grob (Grob_info i)
{
  Item *item = dynamic_cast<Item*> (i.grob_);

  if (item
      && item->internal_has_interface (ly_symbol2scm ("lyric-syllable-interface")))
    {
      if (extender_)
	extender_->set_bound (LEFT, item);

      if (pending_extender_)
	{
	  pending_extender_->set_property ("next", item->self_scm ());
	  completize_extender (pending_extender_);
	  pending_extender_ = 0;
	}
    }
}

void
Extender_engraver::stop_translation_timestep ()
{
  if (extender_ || pending_extender_)
    {
      Context *voice = get_voice_to_lyrics (context ());
      Grob *h = voice ? get_current_note_head (voice) : 0;

      if (h)
	{
	  if (extender_)
	    {
	      Pointer_group_interface::add_grob (extender_,
						 ly_symbol2scm ("heads"), h);
	    }
	  
	  if (pending_extender_)
	    {
	      Pointer_group_interface::add_grob (pending_extender_,
						 ly_symbol2scm ("heads"), h);
	    }
	}
      
      if (extender_)
	{
	  pending_extender_ = extender_;
	  extender_ = 0;
	}
    }

  ev_ = 0;
}

void
completize_extender (Spanner *sp)
{
  if (!sp->get_bound (RIGHT))
    {
      SCM heads = sp->get_property ("heads");
      if (scm_is_pair (heads))
	{
	  Item *it = dynamic_cast<Item*> (unsmob_grob (scm_car (heads)));
	  if (it)
	    sp->set_bound (RIGHT, it);
	}
    }
}

void
Extender_engraver::finalize ()
{
  if (extender_)
    {
      completize_extender (extender_);

      if (!extender_->get_bound (RIGHT))
	extender_->warning (_ ("unterminated extender"));
      extender_ = 0;
    }

  if (pending_extender_)
    {
      completize_extender (pending_extender_);

      if (!pending_extender_->get_bound (RIGHT))
	pending_extender_->warning (_ ("unterminated extender"));
      pending_extender_ = 0;
    }
}


ADD_TRANSLATOR (Extender_engraver,
/* descr */       "Create lyric extenders",
/* creats*/       "LyricExtender",
/* accepts */     "extender-event",
/* acks  */       "lyric-syllable-interface",
/* reads */       "",
/* write */       "");
