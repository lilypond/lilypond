/*
  hyphen-engraver.cc -- implement Hyphen_engraver

  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Glen Prideaux <glenprideaux@iname.com>,
                  Han-Wen Nienhuys <hanwen@cs.uu.nl>,
                  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "warn.hh"
#include "item.hh"
#include "engraver.hh"
#include "spanner.hh"

class Hyphen_engraver : public Engraver
{
  Music *ev_;
  Spanner *hyphen_;
  Spanner *finished_hyphen_;  
public:
  TRANSLATOR_DECLARATIONS (Hyphen_engraver);

protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void finalize ();
  virtual bool try_music (Music*);
  virtual void stop_translation_timestep ();
  virtual void process_music ();
private:

};




Hyphen_engraver::Hyphen_engraver ()
{
  hyphen_ = 0;
  finished_hyphen_ = 0;
  ev_ = 0;
}

void
Hyphen_engraver::acknowledge_grob (Grob_info i)
{
  Item * item =  dynamic_cast<Item*> (i.grob_);
  // -> Text_item
  if (item && item->internal_has_interface (ly_symbol2scm ("lyric-syllable-interface")))
    {
      if (hyphen_)
	hyphen_->set_bound (LEFT, item);

      if (finished_hyphen_)
	finished_hyphen_->set_bound (RIGHT, item);
    }
}


bool
Hyphen_engraver::try_music (Music* r)
{
  if (ev_)
    return false;

  ev_ = r;
  return true;
}

void
completize_hyphen (Spanner* sp)
{
  if (!sp->get_bound (RIGHT))
    {
      SCM heads = sp->get_property ("heads");
      if (scm_is_pair (heads))
	{
	  Item* it = dynamic_cast<Item*> (unsmob_grob (scm_car (heads)));
	  if (it)
	    sp->set_bound (RIGHT, it);
	}
    }
}

  

void
Hyphen_engraver::finalize ()
{
  if (hyphen_)
    {
      completize_hyphen (hyphen_);

      if (!hyphen_->get_bound (RIGHT))
	{
	  hyphen_->warning (_ ("removing unterminated hyphen"));
	  hyphen_->suicide ();
	}

      hyphen_ = 0;
    }

  if (finished_hyphen_)
    {
      completize_hyphen (finished_hyphen_);

      if (!finished_hyphen_->get_bound (RIGHT))
	{
	  finished_hyphen_->warning (_("unterminated hyphen; removing"));
	  finished_hyphen_->suicide ();
	}
      finished_hyphen_ = 0;
    }
}

void
Hyphen_engraver::process_music ()
{
  if (ev_)
    {
      hyphen_ = make_spanner ("LyricHyphen", ev_->self_scm ()
);
    }
}


void
Hyphen_engraver::stop_translation_timestep ()
{
  if (finished_hyphen_ && finished_hyphen_->get_bound (RIGHT))
    {
      finished_hyphen_ = 0;
    }

  if (finished_hyphen_ && hyphen_)
    {
      programming_error ("Haven't finished hyphen yet.");
      finished_hyphen_ = 0;
    }
  
  if (hyphen_)
    finished_hyphen_ = hyphen_;
  hyphen_ = 0;

  ev_ = 0;
}




ENTER_DESCRIPTION (Hyphen_engraver,
/* descr */       "Create lyric hyphens",
/* creats*/       "LyricHyphen",
/* accepts */     "hyphen-event",
/* acks  */      "lyric-syllable-interface",
/* reads */       "",
/* write */       "");
