/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "engraver.hh"

#include "context.hh"
#include "grob-array.hh"
#include "item.hh"
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
  Item *text_ = nullptr;
  int alternative_starting_bar_number_ = 0;
  int alternative_number_ = INT_MIN;
  int alternative_number_increment_ = 0;
  Stream_event *alternative_event_ = nullptr;
  bool considered_numbering_ = false;
  bool must_consider_numbering_ = false;

protected:
  void stop_translation_timestep ();
  void listen_alternative (Stream_event *);
  void acknowledge_bar_line (Grob_info);
  void process_acknowledged ();
  void process_music ();
  void consider_numbering ();
  void create_items ();
  TRANSLATOR_DECLARATIONS (Bar_number_engraver);
};

void
Bar_number_engraver::listen_alternative (Stream_event *ev)
{
  if (alternative_event_)
    return;

  alternative_event_ = ev;
  int current_barnumber = from_scm (get_property (this, "currentBarNumber"), 0);
  Direction alternative_dir = from_scm (get_property (ev, "alternative-dir"), CENTER);
  bool make_alternative = scm_is_eq (get_property (this, "alternativeNumberingStyle"),
                                     ly_symbol2scm ("numbers"))
                          || scm_is_eq (get_property (this, "alternativeNumberingStyle"),
                                        ly_symbol2scm ("numbers-with-letters"));
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

      set_property (context (), "currentBarNumber", to_scm (current_barnumber));
    }
}

void
Bar_number_engraver::process_music ()
{
  // TODO: whichBar doesn't necessarily have its final value until
  // Repeat_acknowledge_engraver::process_music () has run.  Ideally, this
  // engraver would rely on acknowledge_bar_line () to learn when there are bar
  // lines in enclosed contexts, but removing this early check led to
  // differences in horizontal and vertical spacing in a few regression tests.
  // Some things might be sensitive to the order in which grobs are created.
  if (scm_is_string (get_property (this, "whichBar")))
    consider_numbering ();
}

void
Bar_number_engraver::process_acknowledged ()
{
  if (must_consider_numbering_)
    consider_numbering ();
}

void
Bar_number_engraver::consider_numbering ()
{
  if (!considered_numbering_)
    {
      considered_numbering_ = true;

      Moment mp (robust_scm2moment (get_property (this, "measurePosition"), Moment (0)));
      SCM bn = get_property (this, "currentBarNumber");
      SCM proc = get_property (this, "barNumberVisibility");
      if (scm_is_number (bn) && ly_is_procedure (proc)
          && from_scm<bool> (scm_call_2 (proc, bn, mp.smobbed_copy ())))
        {
          create_items ();
          SCM alternative_style = get_property (this, "alternativeNumberingStyle");
          if (scm_is_eq (alternative_style, ly_symbol2scm ("numbers-with-letters")))
            {
              if (alternative_event_)
                {
                  Direction alternative_dir = from_scm (get_property (alternative_event_, "alternative-dir"), RIGHT);
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

                  alternative_number_increment_ = from_scm (get_property (alternative_event_, "alternative-increment"), 1);
                }
            }
          SCM formatter = get_property (this, "barNumberFormatter");
          if (ly_is_procedure (formatter))
            set_property (text_, "text", scm_call_4 (formatter,
                                                     bn,
                                                     mp.smobbed_copy (),
                                                     to_scm (alternative_number_),
                                                     context ()->self_scm ()));
        }
    }
}

Bar_number_engraver::Bar_number_engraver (Context *c)
  : Engraver (c)
{
}

void
Bar_number_engraver::acknowledge_bar_line (Grob_info)
{
  must_consider_numbering_ = true;
}

void
Bar_number_engraver::stop_translation_timestep ()
{
  alternative_event_ = nullptr;
  considered_numbering_ = false;
  must_consider_numbering_ = false;
  if (text_)
    {
      set_object (text_, "side-support-elements",
                  grob_list_to_grob_array (get_property (this, "stavesFound")));
      text_ = nullptr;
    }
}

void
Bar_number_engraver::create_items ()
{
  if (text_)
    return;

  text_ = make_item ("BarNumber", SCM_EOL);
}

void
Bar_number_engraver::boot ()
{
  ADD_LISTENER (Bar_number_engraver, alternative);
  ADD_ACKNOWLEDGER (Bar_number_engraver, bar_line);
}

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
