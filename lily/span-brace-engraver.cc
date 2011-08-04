/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011 Bertrand Bordage
                     Mike Solomon

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
#include "arpeggio.hh"
#include "pointer-group-interface.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "item.hh"

/**
   Make braces that span multiple staves.  Catch braces, and span a
   Span_brace over them if we find more than two braces.
*/
class Span_brace_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Span_brace_engraver);
  DECLARE_ACKNOWLEDGER (brace);

protected:
  void process_acknowledged ();
  void stop_translation_timestep ();

private:
  Item *span_brace_;
  vector<Grob*> braces_;
};

Span_brace_engraver::Span_brace_engraver ()
{
  span_brace_ = 0;
}

void
Span_brace_engraver::acknowledge_brace (Grob_info info)
{
  if (info.origin_contexts (this).size ()) // huh? what's this test for?
    braces_.push_back (info.grob ());
}

void
Span_brace_engraver::process_acknowledged ()
{
  /*
    connectBraces is slightly brusque; we should really read a grob
    property of the caught non-span braces. That way, we can have

    both non-connected and connected braces in one pianostaff.

  */
  if (!span_brace_ && braces_.size () > 1
      && to_boolean (get_property ("connectBraces")))
    {
      span_brace_ = make_item ("Brace", SCM_EOL);
      span_brace_->set_property ("cross-staff", SCM_BOOL_T);
    }
}

void
Span_brace_engraver::stop_translation_timestep ()
{
  if (span_brace_)
    {
      /*
        we do this very late, to make sure we also catch `extra'
        side-pos support like accidentals.
      */
      for (vsize j = 0; j < braces_.size (); j++)
        {
          extract_grob_set (braces_[j], "stems", stems);
          for (vsize i = 0; i < stems.size (); i++)
            Pointer_group_interface::add_grob (span_brace_, ly_symbol2scm ("stems"),
                                               stems[i]);

          extract_grob_set (braces_[j], "side-support-elements", sses);
          for (vsize i = 0; i < sses.size (); i++)
            Pointer_group_interface::add_grob (span_brace_, ly_symbol2scm ("side-support-elements"),
                                               sses[i]);

          /*
            we can't kill the children, since we don't want to the
            previous note to bump into the span brace; so we make
            it transparent.  */
          braces_[j]->set_property ("transparent", SCM_BOOL_T);
        }


      span_brace_->set_parent (braces_[0]->get_parent (Y_AXIS), Y_AXIS);
      span_brace_ = 0;
    }
  braces_.clear ();
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Span_brace_engraver, brace);
ADD_TRANSLATOR (Span_brace_engraver,
                /* doc */
                "Make braces that span multiple staves.",

                /* create */
                "Brace ",

                /* read */
                "connectBraces ",

                /* write */
                ""
                );
