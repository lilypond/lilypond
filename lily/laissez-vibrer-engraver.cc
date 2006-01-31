/*
  laissez-vibrer-engraver.cc -- implement Laissez_vibrer_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/


#include "engraver.hh"
#include "item.hh"
#include "pointer-group-interface.hh"

#include "translator.icc"

class Laissez_vibrer_engraver : public Engraver
{

  Music *event_;
  Grob *lv_column_;
  Link_array<Grob> lv_ties_;
  
  void stop_translation_timestep (); 
  DECLARE_ACKNOWLEDGER (note_head);
  
  virtual bool try_music (Music *);
public:
  TRANSLATOR_DECLARATIONS (Laissez_vibrer_engraver);
};

Laissez_vibrer_engraver::Laissez_vibrer_engraver ()
{
  event_ = 0;
  lv_column_ = 0;
}

void
Laissez_vibrer_engraver::stop_translation_timestep ()
{
  event_ = 0;
  lv_column_ = 0;
  lv_ties_.clear ();
}

bool
Laissez_vibrer_engraver::try_music (Music *m)
{
  event_ = m;
  return true;
}

void
Laissez_vibrer_engraver::acknowledge_note_head (Grob_info inf)
{
  if (!event_)
    return;

  if (!lv_column_)
    {
      lv_column_ = make_item ("LaissezVibrerTieColumn", event_->self_scm ());
    }
  
  Grob *lv_tie = make_item ("LaissezVibrerTie", event_->self_scm ());
  lv_tie->set_object ("note-head", inf.grob ()->self_scm ());
  
  Pointer_group_interface::add_grob (lv_column_, ly_symbol2scm ("ties"),
				     lv_tie);
  lv_tie->set_parent (lv_column_, Y_AXIS);

  lv_ties_.push_back (lv_tie);
}



ADD_ACKNOWLEDGER (Laissez_vibrer_engraver, note_head);
ADD_TRANSLATOR (Laissez_vibrer_engraver, 
		/* doc */ "Create Laissez vibrer items.",
		
		/* create */
		"LaissezVibrerTie "
		"LaissezVibrerTieColumn",

		/* accept */ "laissez-vibrer-event",
		/* read */ "",
		/* write */ "");
