/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2021 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "axis-group-interface.hh"
#include "context.hh"
#include "grob-array.hh"
#include "item.hh"
#include "side-position-interface.hh"
#include "stream-event.hh"
#include "spanner.hh"

#include "translator.icc"

/*
  TODO: detect the top staff (stavesFound), and acknowledge staff-group
  system-start-delims. If we find these, and the top staff is in the
  staff-group, add padding to the bar number.
*/
class Bar_number_engraver : public Engraver
{
protected:
  // A regular bar number.
  Item *text_ = nullptr;
  // A centered bar number.
  Spanner *span_ = nullptr;
  bool considered_numbering_ = false;
  bool must_consider_numbering_ = false;

protected:
  void stop_translation_timestep ();
  void create_bar_number (SCM);
  void acknowledge_bar_line (Grob_info_t<Item>);
  void process_acknowledged ();
  void consider_numbering ();
  int get_alt_number ();
  TRANSLATOR_DECLARATIONS (Bar_number_engraver);
};


void
Bar_number_engraver::create_bar_number (SCM text)
{
  if (scm_is_true (get_property (this, "centerBarNumbers")))
    {
      Grob *column = unsmob<Grob> (get_property (this, "currentCommandColumn"));
      span_ = make_spanner ("CenteredBarNumber", SCM_EOL);
      span_->set_bound (LEFT, column);
      set_property (span_, "text", text);
    }
  else
    {
      text_ = make_item ("BarNumber", SCM_EOL);
      set_property (text_, "text", text);
    }
}

void
Bar_number_engraver::process_acknowledged ()
{
  if (!considered_numbering_ && must_consider_numbering_)
    {
      considered_numbering_ = true;

      // Time to terminate the previous spanner if applicable.
      if (span_)
        {
          span_->set_bound (RIGHT, unsmob<Grob> (get_property (this, "currentCommandColumn")));
          announce_end_grob (span_, SCM_EOL);
          span_ = nullptr;
        }

      SCM vis_p = get_property (this, "barNumberVisibility");
      if (ly_is_procedure (vis_p))
        {
          SCM bn = get_property (this, "currentBarNumber");
          if (scm_is_number (bn))
            {
              const auto mp (from_scm (get_property (this, "measurePosition"),
                                       Moment (0)));

              if (from_scm<bool> (scm_call_2 (vis_p, bn, mp.smobbed_copy ())))
                {
                  SCM formatter = get_property (this, "barNumberFormatter");
                  SCM formatted_text = SCM_EOL;
                  if (ly_is_procedure (formatter))
                    {
                      formatted_text = scm_call_4 (formatter,
                                                   bn,
                                                   mp.smobbed_copy (),
                                                   to_scm (get_alt_number () - 1),
                                                   context ()->self_scm ());
                    }
                  create_bar_number (formatted_text);
                }
            }
        }
    }
}

Bar_number_engraver::Bar_number_engraver (Context *c)
  : Engraver (c)
{
}

void
Bar_number_engraver::acknowledge_bar_line (Grob_info_t<Item>)
{
  must_consider_numbering_ = true;
}

void
Bar_number_engraver::stop_translation_timestep ()
{
  considered_numbering_ = false;
  must_consider_numbering_ = false;
  if (text_)
    {
      set_object (text_, "side-support-elements",
                  grob_list_to_grob_array (get_property (this, "stavesFound")));
      text_ = nullptr;
    }
}

int
Bar_number_engraver::get_alt_number ()
{
  auto alt_num = from_scm (get_property (this, "alternativeNumber"), 0);

  // TODO: Here we have things baked into C++ that should probably be done in
  // Scheme.  Why not just pass the alternative number to the formatter and let
  // the formatter decide whether to use it?  The formatter could look up the
  // style itself, if necessary; well, it could look up the alternative number
  // too.  The impact on users who might have created their own formatters
  // should be considered before changing this.
  if ((alt_num > 0)
      && !scm_is_eq (get_property (this, "alternativeNumberingStyle"),
                     ly_symbol2scm ("numbers-with-letters")))
    {
      alt_num = 0;
    }

  return alt_num;
}

void
Bar_number_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Bar_number_engraver, bar_line);
}

ADD_TRANSLATOR (Bar_number_engraver,
                /* doc */
                "A bar number may be created at any bar line, subject to the"
                " @code{barNumberVisibility} callback.  By default, it is put"
                " on top of all staves and appears only at the left side of"
                " the staff.  "
                "The staves are taken from @code{stavesFound}, which is "
                " maintained by @ref{Staff_collecting_engraver}.  "
                "This engraver usually creates @code{BarNumber} grobs,"
                " but when @code{centerBarNumbers} is true, it makes"
                " @code{CenteredBarNumber} grobs instead.",

                /* create */
                "BarNumber "
                "CenteredBarNumber ",

                /* read */
                "alternativeNumber "
                "alternativeNumberingStyle "
                "barNumberFormatter "
                "barNumberVisibility "
                "centerBarNumbers "
                "currentBarNumber "
                "currentCommandColumn "
                "measurePosition "
                "stavesFound ",

                /* write */
                ""
               );
