/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "paper-column.hh"
#include "output-def.hh"
#include "side-position-interface.hh"
#include "engraver.hh"
#include "context.hh"
#include "grob-array.hh"

#include "translator.icc"

/*
  TODO: detect the top staff (stavesFound), and acknowledge staff-group
  system-start-delims. If we find these, and the top staff is in the
  staff-group, add padding to the bar number.
*/
class Bar_number_engraver : public Engraver
{
protected:
  Item *text_;
protected:
  void stop_translation_timestep ();
  DECLARE_ACKNOWLEDGER (break_aligned);
  DECLARE_ACKNOWLEDGER (break_alignment);
  void process_music ();
  void create_items ();
  TRANSLATOR_DECLARATIONS (Bar_number_engraver);
};

void
Bar_number_engraver::process_music ()
{
  SCM wb = get_property ("whichBar");

  if (scm_is_string (wb))
    {
      Moment mp (robust_scm2moment (get_property ("measurePosition"), Moment (0)));
      if (mp.main_part_ == Rational (0))
	{
	  SCM bn = get_property ("currentBarNumber");
	  SCM proc = get_property ("barNumberVisibility");
	  if (scm_is_number (bn) && ly_is_procedure (proc)
	      && to_boolean (scm_call_1 (proc, bn)))
	    {
	      create_items ();
	      // guh.
	      text_->set_property
		("text", scm_number_to_string (bn, scm_from_int (10)));
	    }
	}
    }
}

Bar_number_engraver::Bar_number_engraver ()
{
  text_ = 0;
}


/*
  see rehearsal mark comments.
 */
void
Bar_number_engraver::acknowledge_break_aligned (Grob_info inf)
{
  Grob *s = inf.grob ();
  if (text_
      && !text_->get_parent (X_AXIS)
      && dynamic_cast<Item *> (s)
      && (s->get_property_data ("break-align-symbol")
	  == text_->get_property_data ("break-align-symbol")))
    {
      /*
	By default this would land on the Paper_column -- so why
	doesn't it work when you leave this out?  */
      text_->set_parent (s, X_AXIS);
    }
}


void
Bar_number_engraver::acknowledge_break_alignment (Grob_info inf)
{
  Grob *s = inf.grob ();
  if (text_
      && dynamic_cast<Item *> (s))
    {
      text_->set_parent (s, X_AXIS);
    }
}

void
Bar_number_engraver::stop_translation_timestep ()
{
  if (text_)
    {
      text_->set_object ("side-support-elements",
			 grob_list_to_grob_array (get_property ("stavesFound")));
      text_ = 0;
    }
}

void
Bar_number_engraver::create_items ()
{
  if (text_)
    return;

  text_ = make_item ("BarNumber", SCM_EOL);
}


ADD_ACKNOWLEDGER (Bar_number_engraver, break_aligned);
ADD_ACKNOWLEDGER (Bar_number_engraver, break_alignment);

ADD_TRANSLATOR (Bar_number_engraver,
		/* doc */
		"A bar number is created whenever @code{measurePosition} is"
		" zero and when there is a bar line (i.e., when"
		" @code{whichBar} is set).  It is put on top of all staves,"
		" and appears only at the left side of the staff.  The staves"
		" are taken from @code{stavesFound}, which is maintained by"
		" @ref{Staff_collecting_engraver}.",

		/* create */
		"BarNumber ",

		/* read */
		"currentBarNumber "
		"whichBar "
		"stavesFound "
		"barNumberVisibility ",

		/* write */
		""
		);
