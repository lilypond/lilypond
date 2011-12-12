/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2011 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <algorithm> // for reverse

#include "paper-column.hh"
#include "output-def.hh"
#include "side-position-interface.hh"
#include "engraver.hh"
#include "context.hh"
#include "grob-array.hh"
#include "stream-event.hh"

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
  int alternative_starting_bar_number_;
  int alternative_number_;
  int alternative_number_increment_;
  Stream_event *alternative_event_;

protected:
  void stop_translation_timestep ();
  DECLARE_TRANSLATOR_LISTENER (alternative);
  DECLARE_ACKNOWLEDGER (break_alignment);
  void process_music ();
  void create_items ();
  TRANSLATOR_DECLARATIONS (Bar_number_engraver);
};

IMPLEMENT_TRANSLATOR_LISTENER (Bar_number_engraver, alternative);
void
Bar_number_engraver::listen_alternative (Stream_event *ev)
{
  if (alternative_event_)
    return;

  alternative_event_ = ev;
  int current_barnumber = robust_scm2int (get_property ("currentBarNumber"), 0);
  Direction alternative_dir = robust_scm2dir (ev->get_property ("alternative-dir"), CENTER);
  bool make_alternative = get_property ("alternativeNumberingStyle") == ly_symbol2scm ("numbers")
                          || get_property ("alternativeNumberingStyle") == ly_symbol2scm ("numbers-with-letters");
  if (make_alternative)
    {
      /*
        if we're starting the first alternative, we set the starting
        bar number to the current bar number
      */
      if (alternative_dir == LEFT)
        alternative_starting_bar_number_ = current_barnumber;

      /*
        if the alternative is not the last one, we send the
        current bar number back to the alternative bar number.
      */
      if (alternative_dir < RIGHT)
        current_barnumber = alternative_starting_bar_number_;

      context ()->set_property ("currentBarNumber", scm_from_int (current_barnumber));
    }
}

void
Bar_number_engraver::process_music ()
{
  SCM wb = get_property ("whichBar");

  if (scm_is_string (wb))
    {
      Moment mp (robust_scm2moment (get_property ("measurePosition"), Moment (0)));
      SCM bn = get_property ("currentBarNumber");
      SCM proc = get_property ("barNumberVisibility");
      if (scm_is_number (bn) && ly_is_procedure (proc)
          && to_boolean (scm_call_2 (proc, bn, mp.smobbed_copy ())))
        {
          create_items ();
          SCM alternative_style = get_property ("alternativeNumberingStyle");
          string text_tag = "";
          if (alternative_style == ly_symbol2scm ("numbers-with-letters"))
            {
              if (alternative_event_)
                {
                  Direction alternative_dir = robust_scm2dir (alternative_event_->get_property ("alternative-dir"), RIGHT);
                  switch (alternative_dir)
                    {
                    case LEFT:
                      alternative_number_ = 0;
                      break;
                    case CENTER:
                      break;
                    case RIGHT:
                      alternative_number_ = INT_MIN;
                      break;
                    default:
                      assert (false);
                    }
                  alternative_number_ += alternative_number_increment_;

                  alternative_number_increment_ = robust_scm2int (alternative_event_->get_property ("alternative-increment"), 1);
                }
            }
          SCM formatter = get_property ("barNumberFormatter");
          if (ly_is_procedure (formatter))
            text_->set_property ("text", scm_call_4 (formatter,
                                                     bn,
                                                     mp.smobbed_copy (),
                                                     scm_from_int (alternative_number_),
                                                     context ()->self_scm ()));
        }
    }
}

Bar_number_engraver::Bar_number_engraver ()
{
  text_ = 0;
  alternative_starting_bar_number_ = 0;
  alternative_number_increment_ = 0;
  alternative_number_ = INT_MIN;
  alternative_event_ = 0;
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
  alternative_event_ = 0;
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
                "barNumberFormatter "
                "barNumberVisibility "
                "alternativeNumberingStyle ",

                /* write */
                "currentBarNumber "
               );
