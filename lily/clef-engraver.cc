/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Mats Bengtsson <matsb@s3.kth.se>

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

#include "translator.icc"
#include "lily-imports.hh"

#include <cctype>

class Clef_engraver final : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Clef_engraver);

protected:
  void stop_translation_timestep ();
  void process_music ();

  void derived_mark () const override;

private:
  Item *clef_ = nullptr;
  Item *modifier_ = nullptr;

  // trigger a clef at the start since #f != '()
  SCM prev_glyph_ = SCM_BOOL_F;
  SCM prev_cpos_ = SCM_BOOL_F;
  int prev_transposition_ = 0;
  void create_clef ();
  void set_glyph ();
  void inspect_clef_properties ();
};

void
Clef_engraver::derived_mark () const
{
  scm_gc_mark (prev_cpos_);
  scm_gc_mark (prev_glyph_);
}

Clef_engraver::Clef_engraver (Context *c)
  : Engraver (c)
{
}

void
Clef_engraver::set_glyph ()
{
  SCM glyph_sym = ly_symbol2scm ("glyph");
  SCM basic = ly_symbol2scm ("Clef");

  execute_pushpop_property (context (), basic, glyph_sym, SCM_UNDEFINED);
  execute_pushpop_property (context (), basic, glyph_sym,
                            get_property (this, "clefGlyph"));
}

void
Clef_engraver::create_clef ()
{
  if (!clef_)
    {
      Item *c = make_item ("Clef", SCM_EOL);

      clef_ = c;
      SCM cpos = get_property (this, "clefPosition");

      if (scm_is_number (cpos))
        set_property (clef_, "staff-position", cpos);

      const auto transp
        = from_scm (get_property (this, "clefTransposition"), 0);
      if (transp)
        {
          Item *g = make_item ("ClefModifier", SCM_EOL);

          const auto dir = sign (transp);
          const auto abs_transp = std::abs (transp) + 1;

          SCM txt = scm_number_to_string (to_scm (abs_transp), to_scm (10));

          SCM style = get_property (this, "clefTranspositionStyle");

          SCM formatter = get_property (this, "clefTranspositionFormatter");
          if (ly_is_procedure (formatter))
            set_property (g, "text", ly_call (formatter, txt, style));

          Side_position_interface::add_support (g, clef_);

          g->set_y_parent (clef_);
          g->set_x_parent (clef_);
          set_property (g, "direction", to_scm (dir));
          modifier_ = g;
        }
    }
}
void
Clef_engraver::process_music ()
{
  inspect_clef_properties ();
  // Efficiency: only create a clef at points where it might be visible,
  // namely where a break has not been forbidden (yet).
  if (scm_is_string (get_property (this, "clefGlyph"))
      && break_allowed (context ()))
    create_clef ();
}

static void
apply_on_children (Context *context, SCM fun)
{
  ly_call (fun, context->self_scm ());
  for (SCM s = context->children_contexts (); scm_is_pair (s); s = scm_cdr (s))
    apply_on_children (unsmob<Context> (scm_car (s)), fun);
}

void
Clef_engraver::inspect_clef_properties ()
{
  SCM glyph = get_property (this, "clefGlyph");
  SCM clefpos = get_property (this, "clefPosition");
  const auto transposition
    = from_scm (get_property (this, "clefTransposition"), 0);
  SCM force_clef = get_property (this, "forceClef");

  if (scm_is_null (clefpos) || !ly_is_equal (glyph, prev_glyph_)
      || !ly_is_equal (clefpos, prev_cpos_)
      || (transposition != prev_transposition_) || from_scm<bool> (force_clef))
    {
      apply_on_children (context (), Lily::invalidate_alterations);

      set_glyph ();
      if (scm_is_true (prev_cpos_)
          || from_scm<bool> (get_property (this, "firstClef")))
        create_clef ();

      if (clef_)
        set_property (clef_, "non-default", SCM_BOOL_T);

      prev_cpos_ = clefpos;
      prev_glyph_ = glyph;
      prev_transposition_ = transposition;
    }

  if (from_scm<bool> (force_clef))
    {
      auto *const w = where_defined (context (), "forceClef");
      set_property (w, "forceClef", SCM_EOL);
    }
}

void
Clef_engraver::stop_translation_timestep ()
{
  if (clef_)
    {
      if (from_scm<bool> (get_property (clef_, "non-default")))
        {
          SCM vis = get_property (this, "explicitClefVisibility");

          if (scm_is_vector (vis))
            set_property (clef_, "break-visibility", vis);
        }

      clef_ = nullptr;
      modifier_ = nullptr;
    }
}

void
Clef_engraver::boot ()
{
}

ADD_TRANSLATOR (Clef_engraver,
                /* doc */
                R"(
Determine and set reference point for pitches.
                )",

                /* create */
                R"(
Clef
ClefModifier
                )",

                /* read */
                R"(
clefGlyph
clefTransposition
clefTranspositionStyle
clefPosition
explicitClefVisibility
forbidBreak
forceBreak
forceClef
                )",

                /* write */
                R"(

                )");
