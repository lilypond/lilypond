/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
                           Mats Bengtsson <matsb@s3.kth.se>
  Copyright (C) 2010--2022 Reinhold Kainhofer <reinhold@kainhofer.com>

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

#include "item.hh"
#include "context.hh"
#include "staff-symbol-referencer.hh"
#include "engraver.hh"
#include "direction.hh"
#include "side-position-interface.hh"
#include "warn.hh"
#include "international.hh"

#include "translator.icc"

#include <cctype>

class Cue_clef_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Cue_clef_engraver);

protected:
  void stop_translation_timestep ();
  void process_music ();

  void derived_mark () const override;

private:
  Item *clef_;
  Item *modifier_;

  SCM prev_glyph_;
  SCM prev_cpos_;
  SCM prev_transposition_;
  void create_clef ();
  void create_end_clef ();
  void set_glyph ();
  void inspect_clef_properties ();
  void create_clef_modifier (SCM transp, SCM style, SCM formatter);
};

void
Cue_clef_engraver::derived_mark () const
{
  scm_gc_mark (prev_transposition_);
  scm_gc_mark (prev_cpos_);
  scm_gc_mark (prev_glyph_);
}

Cue_clef_engraver::Cue_clef_engraver (Context *c)
  : Engraver (c)
{
  clef_ = 0;
  modifier_ = 0;

  prev_transposition_ = prev_cpos_ = prev_glyph_ = SCM_EOL;
}

void
Cue_clef_engraver::set_glyph ()
{
  SCM glyph_sym = ly_symbol2scm ("glyph");
  SCM basic = ly_symbol2scm ("CueClef");
  execute_pushpop_property (context (), basic, glyph_sym, SCM_UNDEFINED);
  execute_pushpop_property (context (), basic, glyph_sym,
                            get_property (this, "cueClefGlyph"));

  basic = ly_symbol2scm ("CueEndClef");
  execute_pushpop_property (context (), basic, glyph_sym, SCM_UNDEFINED);
  execute_pushpop_property (context (), basic, glyph_sym,
                            get_property (this, "clefGlyph"));
}

void
Cue_clef_engraver::create_clef_modifier (SCM transp, SCM style, SCM formatter)
{
  if (scm_is_number (transp) && from_scm<int> (transp))
    {
      Item *g = make_item ("ClefModifier", SCM_EOL);

      int abs_transp = from_scm<int> (transp);
      int dir = sign (abs_transp);
      abs_transp = abs (abs_transp) + 1;

      SCM txt = scm_number_to_string (to_scm (abs_transp), to_scm (10));

      if (ly_is_procedure (formatter))
        set_property (g, "text", ly_call (formatter, txt, style));

      Side_position_interface::add_support (g, clef_);

      g->set_y_parent (clef_);
      g->set_x_parent (clef_);
      set_property (g, "direction", to_scm (dir));
      modifier_ = g;
    }
}

void
Cue_clef_engraver::create_clef ()
{
  if (!clef_)
    {
      Item *c = make_item ("CueClef", SCM_EOL);

      clef_ = c;
      SCM cpos = get_property (this, "cueClefPosition");
      if (scm_is_number (cpos))
        set_property (clef_, "staff-position", cpos);

      create_clef_modifier (
        get_property (this, "cueClefTransposition"),
        get_property (this, "cueClefTranspositionStyle"),
        get_property (this, "cueClefTranspositionFormatter"));
    }
}

void
Cue_clef_engraver::create_end_clef ()
{
  if (!clef_)
    {
      clef_ = make_item ("CueEndClef", SCM_EOL);
      SCM cpos = get_property (this, "clefPosition");
      if (scm_is_number (cpos))
        set_property (clef_, "staff-position", cpos);

      create_clef_modifier (get_property (this, "clefTransposition"),
                            get_property (this, "clefTranspositionStyle"),
                            get_property (this, "clefTranspositionFormatter"));
    }
}

void
Cue_clef_engraver::process_music ()
{
  inspect_clef_properties ();
  // Efficiency: don't create a default clef if it's not going to be
  // visible.  A default clef can only be visible at the start of the
  // line.
  if (scm_is_string (get_property (this, "cueClefGlyph"))
      && break_allowed (context ()))
    create_clef ();
}

void
Cue_clef_engraver::inspect_clef_properties ()
{
  SCM glyph = get_property (this, "cueClefGlyph");
  SCM clefpos = get_property (this, "cueClefPosition");
  SCM transposition = get_property (this, "cueClefTransposition");

  if (!ly_is_equal (glyph, prev_glyph_) || !ly_is_equal (clefpos, prev_cpos_)
      || !ly_is_equal (transposition, prev_transposition_))
    {
      set_glyph ();
      if (scm_is_string (glyph))
        {
          create_clef ();
          if (clef_)
            set_property (clef_, "non-default", SCM_BOOL_T);
        }
      else
        create_end_clef ();

      prev_cpos_ = clefpos;
      prev_glyph_ = glyph;
      prev_transposition_ = transposition;
    }
}

void
Cue_clef_engraver::stop_translation_timestep ()
{
  if (clef_)
    {
      if (from_scm<bool> (get_property (clef_, "non-default")))
        {
          SCM vis = get_property (this, "explicitCueClefVisibility");

          if (scm_is_vector (vis))
            set_property (clef_, "break-visibility", vis);
        }

      clef_ = 0;
      modifier_ = 0;
    }
}

void
Cue_clef_engraver::boot ()
{
}

ADD_TRANSLATOR (Cue_clef_engraver,
                /* doc */
                R"(
Determine and set reference point for pitches in cued voices.
                )",

                /* create */
                R"(
CueClef
CueEndClef
ClefModifier
                )",

                /* read */
                R"(
cueClefGlyph
cueClefTransposition
cueClefTranspositionStyle
cueClefPosition
explicitCueClefVisibility
forbidBreak
forceBreak
middleCCuePosition
clefTransposition
                )",

                /* write */
                R"(

                )");
