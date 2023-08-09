/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef TEMPLATE_ENGRAVER_FOR_BEAMS_HH
#define TEMPLATE_ENGRAVER_FOR_BEAMS_HH

#include "beaming-pattern.hh"
#include "engraver.hh"
#include "moment.hh"
#include "spanner.hh"

class Template_engraver_for_beams : public Engraver
{
protected:
  using Engraver::Engraver;

  Spanner *finished_beam_ = nullptr;

  Beaming_pattern *beam_pattern_ = nullptr;
  Beaming_pattern *finished_beam_pattern_ = nullptr;

  // position within measure where beam started.
  Moment beam_start_position_;

  // moment (global time) where beam started.
  Moment beam_start_moment_;

  Moment last_added_moment_;

  Beaming_options beaming_options_;
  Beaming_options finished_beaming_options_;

  virtual void derived_mark () const override;

  void typeset_beam ();

  void begin_beam ();
  void add_stem (Item *, Duration const &);
};

#endif // TEMPLATE_ENGRAVER_FOR_BEAMS_HH
