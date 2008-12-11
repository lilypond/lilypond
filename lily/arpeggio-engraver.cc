/*
  arpeggio-engraver.cc -- implement Arpeggio_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2008 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "engraver.hh"

#include "pointer-group-interface.hh"
#include "arpeggio.hh"
#include "stem.hh"
#include "rhythmic-head.hh"
#include "side-position-interface.hh"
#include "stream-event.hh"
#include "note-column.hh"
#include "item.hh"

#include "translator.icc"

class Arpeggio_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Arpeggio_engraver);

  void acknowledge_stem (Grob_info);
  void acknowledge_rhythmic_head (Grob_info);
protected:
  void process_music ();
  void stop_translation_timestep ();
  DECLARE_TRANSLATOR_LISTENER (arpeggio);
private:
  Item *arpeggio_;
  Stream_event *arpeggio_event_;
};

Arpeggio_engraver::Arpeggio_engraver ()
{
  arpeggio_ = 0;
  arpeggio_event_ = 0;
}

IMPLEMENT_TRANSLATOR_LISTENER (Arpeggio_engraver, arpeggio);
void Arpeggio_engraver::listen_arpeggio (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (arpeggio_event_, ev);
}

void
Arpeggio_engraver::acknowledge_stem (Grob_info info)
{
  if (arpeggio_)
    {
      if (!arpeggio_->get_parent (Y_AXIS))
	arpeggio_->set_parent (info.grob (), Y_AXIS);

      Pointer_group_interface::add_grob (arpeggio_,
					 ly_symbol2scm ("stems"),
					 info.grob ());
    }
}
void
Arpeggio_engraver::acknowledge_rhythmic_head (Grob_info info)
{
  if (arpeggio_)

    /*
      We can't catch local key items (accidentals) from Voice context,
      see Local_key_engraver
    */
    Side_position_interface::add_support (arpeggio_, info.grob ());
}

void
Arpeggio_engraver::process_music ()
{
  if (arpeggio_event_)
    {
      arpeggio_ = make_item ("Arpeggio", arpeggio_event_->self_scm ());
    }
}

void
Arpeggio_engraver::stop_translation_timestep ()
{
  arpeggio_ = 0;
  arpeggio_event_ = 0;
}

ADD_ACKNOWLEDGER (Arpeggio_engraver, stem);
ADD_ACKNOWLEDGER (Arpeggio_engraver, rhythmic_head);

ADD_TRANSLATOR (Arpeggio_engraver,
		/* doc */
		"Generate an Arpeggio symbol.",

		/* create */
		"Arpeggio",

		/* read */
		"",

		/* write */
		""
		);
