/*
  part-combine-engraver.cc -- implement PC-engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2009 Jan Nieuwenhuizen <janneke@gnu.org>

  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"
#include "multi-measure-rest.hh"
#include "note-head.hh"
#include "side-position-interface.hh"
#include "stem.hh"
#include "stream-event.hh"
#include "text-interface.hh"
#include "item.hh"

#include "translator.icc"

class Part_combine_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Part_combine_engraver);

protected:
  DECLARE_ACKNOWLEDGER (note_head);
  DECLARE_ACKNOWLEDGER (stem);

  DECLARE_TRANSLATOR_LISTENER (part_combine);
  void process_music ();
  void stop_translation_timestep ();
private:
  Item *text_;
  Stream_event *event_;
};

IMPLEMENT_TRANSLATOR_LISTENER (Part_combine_engraver, part_combine);
void
Part_combine_engraver::listen_part_combine (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (event_, ev);
}

Part_combine_engraver::Part_combine_engraver ()
{
  text_ = 0;
  event_ = 0;
}

void
Part_combine_engraver::process_music ()
{
  if (event_
      && to_boolean (get_property ("printPartCombineTexts")))
    {
      SCM what = event_->get_property ("class");
      SCM text = SCM_EOL;
      if (what == ly_symbol2scm ("solo-one-event"))
	text = get_property ("soloText");
      else if (what == ly_symbol2scm ("solo-two-event"))
	text = get_property ("soloIIText");
      else if (what == ly_symbol2scm ("unisono-event"))
	text = get_property ("aDueText");

      if (Text_interface::is_markup (text))
	{
	  text_ = make_item ("CombineTextScript", event_->self_scm ());
	  text_->set_property ("text", text);
	}
    }
}

void
Part_combine_engraver::acknowledge_note_head (Grob_info i)
{
  if (text_)
    {
      Grob *t = text_;
      Side_position_interface::add_support (t, i.grob ());
      if (Side_position_interface::get_axis (t) == X_AXIS
	  && !t->get_parent (Y_AXIS))
	t->set_parent (i.grob (), Y_AXIS);
    }
}

void
Part_combine_engraver::acknowledge_stem (Grob_info i)
{
  if (text_)
    Side_position_interface::add_support (text_, i.grob ());
}

void
Part_combine_engraver::stop_translation_timestep ()
{
  text_ = 0;
  event_ = 0;
}

ADD_ACKNOWLEDGER (Part_combine_engraver, note_head);
ADD_ACKNOWLEDGER (Part_combine_engraver, stem);
ADD_TRANSLATOR (Part_combine_engraver,
		/* doc */
		"Part combine engraver for orchestral scores: Print markings"
		" @q{a2}, @q{Solo}, @q{Solo II}, and @q{unisono}.",

		/* create */
		"CombineTextScript ",

		/* read */
		"printPartCombineTexts "
		"soloText "
		"soloIIText "
		"aDueText ",

		/* write */
		""
		);
