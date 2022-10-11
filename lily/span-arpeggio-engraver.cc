/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Jan Nieuwenhuizen <janneke@gnu.org>

  Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "item.hh"
#include "pointer-group-interface.hh"
#include "separation-item.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"

#include "translator.icc"

using std::vector;

/**
   Make arpeggios that span multiple staves.  Catch arpeggios, and span a
   Span_arpeggio over them if we find more than two arpeggios.
*/
class Span_arpeggio_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Span_arpeggio_engraver);
  void acknowledge_arpeggio (Grob_info);
  void acknowledge_note_column (Grob_info_t<Item>);

protected:
  void process_acknowledged ();
  void stop_translation_timestep ();

private:
  Item *span_arpeggio_;
  vector<Grob *> arpeggios_;
  vector<Grob *> note_columns_;
};

Span_arpeggio_engraver::Span_arpeggio_engraver (Context *c)
  : Engraver (c)
{
  span_arpeggio_ = 0;
}

void
Span_arpeggio_engraver::acknowledge_arpeggio (Grob_info info)
{
  arpeggios_.push_back (info.grob ());
}

void
Span_arpeggio_engraver::acknowledge_note_column (Grob_info_t<Item> info)
{
  note_columns_.push_back (info.grob ());
}

void
Span_arpeggio_engraver::process_acknowledged ()
{
  /*
    connectArpeggios is slightly brusque; we should really read a grob
    property of the caught non-span arpeggios. That way, we can have

    both non-connected and connected arps in one pianostaff.

  */
  if (!span_arpeggio_ && arpeggios_.size () > 1
      && from_scm<bool> (get_property (this, "connectArpeggios")))
    span_arpeggio_ = make_item ("Arpeggio", SCM_EOL);

  if (span_arpeggio_)
    {
      for (vsize i = 0; i < note_columns_.size (); i++)
        Separation_item::add_conditional_item (note_columns_[i],
                                               span_arpeggio_);
      note_columns_.clear ();
    }
}

void
Span_arpeggio_engraver::stop_translation_timestep ()
{
  if (span_arpeggio_)
    {
      /*
        we do this very late, to make sure we also catch `extra'
        side-pos support like accidentals.
      */
      for (vsize j = 0; j < arpeggios_.size (); j++)
        {
          extract_grob_set (arpeggios_[j], "stems", stems);
          for (vsize i = 0; i < stems.size (); i++)
            Pointer_group_interface::add_grob (
              span_arpeggio_, ly_symbol2scm ("stems"), stems[i]);

          extract_grob_set (arpeggios_[j], "side-support-elements", sses);
          for (vsize i = 0; i < sses.size (); i++)
            Pointer_group_interface::add_grob (
              span_arpeggio_, ly_symbol2scm ("side-support-elements"), sses[i]);

          /*
            we can't kill the children, since we don't want to the
            previous note to bump into the span arpeggio; so we make
            it transparent.  */
          set_property (arpeggios_[j], "transparent", SCM_BOOL_T);
        }

      span_arpeggio_->set_y_parent (arpeggios_[0]->get_y_parent ());
      span_arpeggio_ = 0;
    }
  arpeggios_.clear ();
  note_columns_.clear ();
}

void
Span_arpeggio_engraver::boot ()
{
  ADD_ACKNOWLEDGER (arpeggio);
  ADD_ACKNOWLEDGER (note_column);
}

ADD_TRANSLATOR (Span_arpeggio_engraver,
                /* doc */
                R"(
Make arpeggios that span multiple staves.
                )",

                /* create */
                R"(
Arpeggio
                )",

                /* read */
                R"(
connectArpeggios
                )",

                /* write */
                R"(

                )");
