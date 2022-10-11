/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "align-interface.hh"
#include "axis-group-interface.hh"
#include "engraver.hh"
#include "page-layout-problem.hh"
#include "pointer-group-interface.hh"
#include "side-position-interface.hh"
#include "system.hh"
#include "text-interface.hh"

#include "translator.icc"

using std::vector;

class Instrument_name_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Instrument_name_engraver);

protected:
  Spanner *text_spanner_;

  SCM long_text_;
  SCM short_text_;

  vector<Grob *> axis_groups_;
  vector<Grob *> backup_axis_groups_;

  void finalize () override;
  void acknowledge_hara_kiri_group_spanner (Grob_info);
  void process_music ();
  void start_spanner ();
  void consider_start_spanner ();
  void stop_spanner ();

  void derived_mark () const override;
};

void
Instrument_name_engraver::derived_mark () const
{
  scm_gc_mark (long_text_);
  scm_gc_mark (short_text_);
}

Instrument_name_engraver::Instrument_name_engraver (Context *c)
  : Engraver (c)
{
  text_spanner_ = 0;

  long_text_ = SCM_EOL;
  short_text_ = SCM_EOL;
}

void
Instrument_name_engraver::process_music ()
{
  consider_start_spanner ();
}

void
Instrument_name_engraver::consider_start_spanner ()
{
  SCM long_text = get_property (this, "instrumentName");
  SCM short_text = get_property (this, "shortInstrumentName");

  if (!(Text_interface::is_markup (long_text)
        || Text_interface::is_markup (short_text)))
    {
      long_text = get_property (this, "vocalName");
      short_text = get_property (this, "shortVocalName");
    }

  if ((Text_interface::is_markup (long_text)
       || Text_interface::is_markup (short_text))
      && (!text_spanner_ || !scm_is_eq (short_text_, short_text)
          || !scm_is_eq (long_text_, long_text)))
    {
      if (text_spanner_)
        stop_spanner ();

      short_text_ = short_text;
      long_text_ = long_text;

      start_spanner ();
    }
}

void
Instrument_name_engraver::start_spanner ()
{
  text_spanner_ = make_spanner ("InstrumentName", SCM_EOL);

  auto *col = unsmob<Grob> (get_property (this, "currentCommandColumn"));
  text_spanner_->set_bound (LEFT, col);
  set_property (text_spanner_, "text", short_text_);
  set_property (text_spanner_, "long-text", long_text_);

  /*
    UGH, should handle this in Score_engraver.
  */
  Grob *system = unsmob<Grob> (get_property (this, "rootSystem"));
  if (system)
    Axis_group_interface::add_element (system, text_spanner_);
  else
    text_spanner_->programming_error ("cannot find root system");
}

void
Instrument_name_engraver::acknowledge_hara_kiri_group_spanner (Grob_info info)
{
  if (Page_layout_problem::is_spaceable (info.grob ()))
    axis_groups_.push_back (info.grob ());
  else
    {
      // By default, don't include non-spaceable staves in the
      // support of an instrument name.  However, if the only staves
      // are non-spaceable, we'll fall back to using them.
      backup_axis_groups_.push_back (info.grob ());
    }
}

void
Instrument_name_engraver::finalize ()
{
  if (text_spanner_)
    stop_spanner ();
}

void
Instrument_name_engraver::stop_spanner ()
{
  if (axis_groups_.empty ())
    axis_groups_ = backup_axis_groups_;

  for (vsize i = 0; i < axis_groups_.size (); i++)
    Pointer_group_interface::add_grob (
      text_spanner_, ly_symbol2scm ("elements"), axis_groups_[i]);

  auto *col = unsmob<Grob> (get_property (this, "currentCommandColumn"));
  text_spanner_->set_bound (RIGHT, col);

  Pointer_group_interface::set_ordered (text_spanner_,
                                        ly_symbol2scm ("elements"), false);

  text_spanner_ = 0;
}

void
Instrument_name_engraver::boot ()
{
  ADD_ACKNOWLEDGER (hara_kiri_group_spanner);
}

ADD_TRANSLATOR (Instrument_name_engraver,
                /* doc */
                R"(
Create a system start text for instrument or vocal names.
                )",

                /* create */
                R"(
InstrumentName
                )",

                /* read */
                R"(
currentCommandColumn
instrumentName
shortInstrumentName
shortVocalName
vocalName
                )",

                /* write */
                R"(

                )");
