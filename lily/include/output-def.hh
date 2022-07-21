/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef OUTPUT_DEF_HH
#define OUTPUT_DEF_HH

#include "lily-proto.hh"
#include "virtual-methods.hh"
#include "smobs.hh"
#include "input.hh"

/*
  Output settings for a block of music.

  This devolved into a rather empty class. The distinction between
  various instances is made in the parser, which creates
  midi/layout/paper blocks depending on the keyword read.

  The data structure is set up as recursive: the definitions not
  supplied in layout are looked up in paper. This is done through
  the parent_ field of Output_def.

 */
class Output_def : public Smob<Output_def>
{
public:
  int print_smob (SCM, scm_print_state *) const;
  SCM mark_smob () const;
  static const char *const type_p_name_;
  virtual ~Output_def ();
  VIRTUAL_CLASS_NAME (Output_def);
  virtual Output_def *clone () const { return new Output_def (*this); }

  SCM scope_;
  Output_def *parent_;

  Input input_origin_;

  Output_def (Output_def const &);
  Output_def ();

  /*
    variables.
   */
  SCM c_variable (const std::string &id) const;
  SCM lookup_variable (SCM sym) const;
  void set_variable (SCM sym, SCM val);
  void normalize ();
  Real get_dimension (SCM symbol) const;
};
SCM get_font_table (Output_def *def);
void assign_context_def (Output_def *m, SCM transdef);
SCM find_context_def (Output_def const *m, SCM name);

Interval line_dimension_interval (Output_def *def, vsize n);

Font_metric *select_encoded_font (Output_def *layout, SCM chain);
Font_metric *select_font (Output_def *layout, SCM chain);

Font_metric *find_pango_font (Output_def *layout, SCM descr, Real factor);
Font_metric *find_scaled_font (Output_def *od, Font_metric *f,
                               Real magnification);
Output_def *scale_output_def (Output_def *def, Real scale);

Real output_scale (Output_def *);

#endif /* OUTPUT_DEF_HH */
