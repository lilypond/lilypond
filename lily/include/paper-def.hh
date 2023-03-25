/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef PAPER_DEF_HH
#define PAPER_DEF_HH

#include "lily-guile.hh"
#include "output-def.hh"
#include "real.hh"

#include <pango/pango.h>

Font_metric *find_pango_font (Output_def *layout,
                              PangoFontDescription *description, Real factor);
Font_metric *find_scaled_font (Output_def *od, Font_metric *f,
                               Real magnification);
Output_def *scale_output_def (Output_def *def, Real scale);

Real output_scale (Output_def *);

#endif // PAPER_DEF_HH
