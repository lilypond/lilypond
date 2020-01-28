/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
                           Mats Bengtsson <matsb@s3.kth.se>
  Copyright (C) 2010--2020 Reinhold Kainhofer <reinhold@kainhofer.com>

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

#include <cctype>

#include "context.hh"
#include "direction.hh"
#include "engraver.hh"
#include "international.hh"
#include "item.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "warn.hh"

#include "translator.icc"

class Cue_clef_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Cue_clef_engraver);

protected:
  void stop_translation_timestep ();
  void process_music ();
  void acknowledge_bar_line (Grob_info);

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

Cue_clef_engraver::Cue_clef_engraver (Context *c) : Engraver (c)
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
                            get_property ("cueClefGlyph"));

  basic = ly_symbol2scm ("CueEndClef");
  execute_pushpop_property (context (), basic, glyph_sym, SCM_UNDEFINED);
  execute_pushpop_property (context (), basic, glyph_sym,
                            get_property ("clefGlyph"));
}

/**
   Generate a clef at the start of a measure. (when you see a Bar,
   ie. a breakpoint)
*/
void
Cue_clef_engraver::acknowledge_bar_line (Grob_info info)
{
  Item *item = dynamic_cast<Item *> (info.grob ());
  if (item && scm_is_string (get_property ("cueClefGlyph")))
    create_clef ();
}

void
Cue_clef_engraver::create_clef_modifier (SCM transp, SCM style, SCM formatter)
{
  if (scm_is_number (transp) && scm_to_int (transp))
    {
      Item *g = make_item ("ClefModifier", SCM_EOL);

      int abs_transp = scm_to_int (transp);
      int dir = sign (abs_transp);
      abs_transp = abs (abs_transp) + 1;

      SCM txt
          = scm_number_to_string (scm_from_int (abs_transp), scm_from_int (10));

      if (ly_is_procedure (formatter))
        g->set_property ("text", scm_call_2 (formatter, txt, style));

      Side_position_interface::add_support (g, clef_);

      g->set_parent (clef_, Y_AXIS);
      g->set_parent (clef_, X_AXIS);
      g->set_property ("direction", scm_from_int (dir));
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
      SCM cpos = get_property ("cueClefPosition");
      if (scm_is_number (cpos))
        clef_->set_property ("staff-position", cpos);

      create_clef_modifier (get_property ("cueClefTransposition"),
                            get_property ("cueClefTranspositionStyle"),
                            get_property ("cueClefTranspositionFormatter"));
    }
}

void
Cue_clef_engraver::create_end_clef ()
{
  if (!clef_)
    {
      clef_ = make_item ("CueEndClef", SCM_EOL);
      SCM cpos = get_property ("clefPosition");
      if (scm_is_number (cpos))
        clef_->set_property ("staff-position", cpos);

      create_clef_modifier (get_property ("clefTransposition"),
                            get_property ("clefTranspositionStyle"),
                            get_property ("clefTranspositionFormatter"));
    }
}

void
Cue_clef_engraver::process_music ()
{
  inspect_clef_properties ();
}

void
Cue_clef_engraver::inspect_clef_properties ()
{
  SCM glyph = get_property ("cueClefGlyph");
  SCM clefpos = get_property ("cueClefPosition");
  SCM transposition = get_property ("cueClefTransposition");

  if (!ly_is_equal (glyph, prev_glyph_) || !ly_is_equal (clefpos, prev_cpos_)
      || !ly_is_equal (transposition, prev_transposition_))
    {
      set_glyph ();
      if (scm_is_string (glyph))
        {
          create_clef ();
          if (clef_)
            clef_->set_property ("non-default", SCM_BOOL_T);
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
      if (to_boolean (clef_->get_property ("non-default")))
        {
          SCM vis = get_property ("explicitCueClefVisibility");

          if (scm_is_vector (vis))
            clef_->set_property ("break-visibility", vis);
        }

      clef_ = 0;
      modifier_ = 0;
    }
}

void
Cue_clef_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Cue_clef_engraver, bar_line);
}

ADD_TRANSLATOR (Cue_clef_engraver,
                /* doc */
                "Determine and set reference point for pitches in cued voices.",

                /* create */
                "CueClef "
                "CueEndClef "
                "ClefModifier ",

                /* read */
                "cueClefGlyph "
                "cueClefTransposition "
                "cueClefTranspositionStyle "
                "cueClefPosition "
                "explicitCueClefVisibility "
                "middleCCuePosition "
                "clefTransposition ",

                /* write */
                "");
