/*
  fall-engraver.cc -- implement Fall_engraver

  (c) 2006 Han-Wen Nienhuys

  
*/

#include "engraver.hh"
#include "item.hh"
#include "spanner.hh"

class Fall_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Fall_engraver);
  DECLARE_ACKNOWLEDGER (note_head);

protected:
  virtual bool try_music (Music *event);
  void process_music ();
  void stop_translation_timestep ();
  void start_translation_timestep ();
  void stop_fall ();
  
private:
  Moment stop_moment_;
  Music *fall_event_;
  Spanner *fall_;
  Grob *note_head_;
};

void
Fall_engraver::stop_fall ()
{
  bool bar = scm_is_string (get_property ("whichBar"));
  
  
  fall_->set_bound (RIGHT, unsmob_grob (
					bar
					? get_property ("currentCommandColumn")
					: get_property ("currentMusicalColumn")));
  fall_ = 0;
  note_head_ = 0;
  fall_event_ = 0;
}

void
Fall_engraver::stop_translation_timestep ()
{
  if (fall_ && !fall_->get_bound (LEFT)) 
    {
      fall_->set_bound (LEFT, note_head_);
      fall_->set_parent (note_head_,  Y_AXIS);
    }
}

void
Fall_engraver::start_translation_timestep ()
{
  if (fall_ && now_mom ().main_part_ >= stop_moment_.main_part_)
    {
      stop_fall ();
    }
}

void
Fall_engraver::acknowledge_note_head (Grob_info info)
{
  if (!fall_event_)
    return;
  
  if (note_head_ && fall_)
    {
      stop_fall ();
    }

  note_head_ = info.grob ();
  stop_moment_ = now_mom () + info.music_cause ()->get_length ();
}

Fall_engraver::Fall_engraver ()
{
  fall_ = 0;
  note_head_ = 0;
  fall_event_ = 0;
}

bool
Fall_engraver::try_music (Music *r)
{
  fall_event_ = r;
  return true;
}

void
Fall_engraver::process_music ()
{
  if (fall_event_ && !fall_)
    {
      fall_ = make_spanner ("BendAfter", fall_event_->self_scm ());
      fall_->set_property ("delta-position",
			   scm_from_double (robust_scm2double (fall_event_->get_property ("delta-pitch"), 0) * 0.5));
    }
}

#include "translator.icc"


ADD_ACKNOWLEDGER (Fall_engraver, note_head);


ADD_TRANSLATOR (Fall_engraver,
		/* doc */ "Create fall spanners.",
		/* create */ "BendAfter",
		/* accept */ "bend-after-event",
		/* read */ "",
		/* write */ "");
