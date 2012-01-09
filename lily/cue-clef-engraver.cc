/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>
                           Mats Bengtsson <matsb@s3.kth.se>
  Copyright (C) 2010--2012 Reinhold Kainhofer <reinhold@kainhofer.com>

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
using namespace std;

#include "item.hh"
#include "context.hh"
#include "bar-line.hh"
#include "staff-symbol-referencer.hh"
#include "engraver.hh"
#include "direction.hh"
#include "side-position-interface.hh"
#include "warn.hh"
#include "international.hh"

#include "translator.icc"

class Cue_clef_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Cue_clef_engraver);

protected:
  void stop_translation_timestep ();
  void process_music ();
  DECLARE_ACKNOWLEDGER (bar_line);

  virtual void derived_mark () const;
private:
  Item *clef_;
  Item *octavate_;

  SCM prev_glyph_;
  SCM prev_cpos_;
  SCM prev_octavation_;
  void create_clef ();
  void create_end_clef ();
  void set_glyph ();
  void inspect_clef_properties ();
  void create_octavate_eight (SCM oct);
};

void
Cue_clef_engraver::derived_mark () const
{
  scm_gc_mark (prev_octavation_);
  scm_gc_mark (prev_cpos_);
  scm_gc_mark (prev_glyph_);
}

Cue_clef_engraver::Cue_clef_engraver ()
{
  clef_ = 0;
  octavate_ = 0;

  prev_octavation_ = prev_cpos_ = prev_glyph_ = SCM_EOL;
}

void
Cue_clef_engraver::set_glyph ()
{
  SCM glyph_sym = ly_symbol2scm ("glyph");
  SCM basic = ly_symbol2scm ("CueClef");
  execute_pushpop_property (context (), basic, glyph_sym, SCM_UNDEFINED);
  execute_pushpop_property (context (), basic, glyph_sym, get_property ("cueClefGlyph"));

  basic = ly_symbol2scm ("CueEndClef");
  execute_pushpop_property (context (), basic, glyph_sym, SCM_UNDEFINED);
  execute_pushpop_property (context (), basic, glyph_sym, get_property ("clefGlyph"));
}

/**
   Generate a clef at the start of a measure. (when you see a Bar,
   ie. a breakpoint)
*/
void
Cue_clef_engraver::acknowledge_bar_line (Grob_info info)
{
  Item *item = info.item ();
  if (item && scm_is_string (get_property ("cueClefGlyph")))
    create_clef ();
}

void
Cue_clef_engraver::create_octavate_eight (SCM oct)
{
  if (scm_is_number (oct) && scm_to_int (oct))
    {
      Item *g = make_item ("OctavateEight", SCM_EOL);

      int abs_oct = scm_to_int (oct);
      int dir = sign (abs_oct);
      abs_oct = abs (abs_oct) + 1;

      SCM txt = scm_number_to_string (scm_from_int (abs_oct),
                                      scm_from_int (10));

      g->set_property ("text",
                       scm_list_n (ly_lily_module_constant ("vcenter-markup"),
                                   txt, SCM_UNDEFINED));
      Side_position_interface::add_support (g, clef_);

      g->set_parent (clef_, Y_AXIS);
      g->set_parent (clef_, X_AXIS);
      g->set_property ("direction", scm_from_int (dir));
      octavate_ = g;
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

      create_octavate_eight (get_property ("cueClefOctavation"));
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

      create_octavate_eight (get_property ("clefOctavation"));
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
  SCM octavation = get_property ("cueClefOctavation");

  if (scm_equal_p (glyph, prev_glyph_) == SCM_BOOL_F
      || scm_equal_p (clefpos, prev_cpos_) == SCM_BOOL_F
      || scm_equal_p (octavation, prev_octavation_) == SCM_BOOL_F)
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
      prev_octavation_ = octavation;
    }

}

void
Cue_clef_engraver::stop_translation_timestep ()
{
  if (clef_)
    {
      SCM vis = 0;
      if (to_boolean (clef_->get_property ("non-default")))
        vis = get_property ("explicitCueClefVisibility");

      if (vis)
        clef_->set_property ("break-visibility", vis);

      clef_ = 0;
      octavate_ = 0;
    }
}

ADD_ACKNOWLEDGER (Cue_clef_engraver, bar_line);
ADD_TRANSLATOR (Cue_clef_engraver,
                /* doc */
                "Determine and set reference point for pitches in cued voices.",

                /* create */
                "CueClef "
                "CueEndClef "
                "OctavateEight ",

                /* read */
                "cueClefGlyph "
                "cueClefOctavation "
                "cueClefPosition "
                "explicitCueClefVisibility "
                "middleCCuePosition "
                "clefOctavation ",

                /* write */
                ""
               );
