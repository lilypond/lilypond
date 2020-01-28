/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

#include <cctype>

#include "context.hh"
#include "direction.hh"
#include "engraver.hh"
#include "item.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"

#include "lily-imports.hh"
#include "translator.icc"

class Clef_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Clef_engraver);

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
  void set_glyph ();
  void inspect_clef_properties ();
};

void
Clef_engraver::derived_mark () const
{
  scm_gc_mark (prev_transposition_);
  scm_gc_mark (prev_cpos_);
  scm_gc_mark (prev_glyph_);
}

Clef_engraver::Clef_engraver (Context *c) : Engraver (c)
{
  clef_ = 0;
  modifier_ = 0;

  /*
    will trigger a clef at the start since #f != ' ()
  */
  prev_transposition_ = prev_cpos_ = prev_glyph_ = SCM_BOOL_F;
}

void
Clef_engraver::set_glyph ()
{
  SCM glyph_sym = ly_symbol2scm ("glyph");
  SCM basic = ly_symbol2scm ("Clef");

  execute_pushpop_property (context (), basic, glyph_sym, SCM_UNDEFINED);
  execute_pushpop_property (context (), basic, glyph_sym,
                            get_property ("clefGlyph"));
}

/**
   Generate a clef at the start of a measure. (when you see a Bar,
   ie. a breakpoint)
*/
void
Clef_engraver::acknowledge_bar_line (Grob_info info)
{
  Item *item = dynamic_cast<Item *> (info.grob ());
  if (item && scm_is_string (get_property ("clefGlyph")))
    create_clef ();
}

void
Clef_engraver::create_clef ()
{
  if (!clef_)
    {
      Item *c = make_item ("Clef", SCM_EOL);

      clef_ = c;
      SCM cpos = get_property ("clefPosition");

      if (scm_is_number (cpos))
        clef_->set_property ("staff-position", cpos);

      SCM transp = get_property ("clefTransposition");
      if (scm_is_number (transp) && scm_to_int (transp))
        {
          Item *g = make_item ("ClefModifier", SCM_EOL);

          int abs_transp = scm_to_int (transp);
          int dir = sign (abs_transp);
          abs_transp = abs (abs_transp) + 1;

          SCM txt = scm_number_to_string (scm_from_int (abs_transp),
                                          scm_from_int (10));

          SCM style = get_property ("clefTranspositionStyle");

          SCM formatter = get_property ("clefTranspositionFormatter");
          if (ly_is_procedure (formatter))
            g->set_property ("text", scm_call_2 (formatter, txt, style));

          Side_position_interface::add_support (g, clef_);

          g->set_parent (clef_, Y_AXIS);
          g->set_parent (clef_, X_AXIS);
          g->set_property ("direction", scm_from_int (dir));
          modifier_ = g;
        }
    }
}
void
Clef_engraver::process_music ()
{
  inspect_clef_properties ();
}

static void
apply_on_children (Context *context, SCM fun)
{
  scm_call_1 (fun, context->self_scm ());
  for (SCM s = context->children_contexts (); scm_is_pair (s); s = scm_cdr (s))
    apply_on_children (unsmob<Context> (scm_car (s)), fun);
}

void
Clef_engraver::inspect_clef_properties ()
{
  SCM glyph = get_property ("clefGlyph");
  SCM clefpos = get_property ("clefPosition");
  SCM transposition = get_property ("clefTransposition");
  SCM force_clef = get_property ("forceClef");

  if (scm_is_null (clefpos) || !ly_is_equal (glyph, prev_glyph_)
      || !ly_is_equal (clefpos, prev_cpos_)
      || !ly_is_equal (transposition, prev_transposition_)
      || to_boolean (force_clef))
    {
      apply_on_children (context (), Lily::invalidate_alterations);

      set_glyph ();
      if (scm_is_true (prev_cpos_) || to_boolean (get_property ("firstClef")))
        create_clef ();

      if (clef_)
        clef_->set_property ("non-default", SCM_BOOL_T);

      prev_cpos_ = clefpos;
      prev_glyph_ = glyph;
      prev_transposition_ = transposition;
    }

  if (to_boolean (force_clef))
    {
      SCM prev;
      Context *w
          = context ()->where_defined (ly_symbol2scm ("forceClef"), &prev);
      w->set_property ("forceClef", SCM_EOL);
    }
}

void
Clef_engraver::stop_translation_timestep ()
{
  if (clef_)
    {
      if (to_boolean (clef_->get_property ("non-default")))
        {
          SCM vis = get_property ("explicitClefVisibility");

          if (scm_is_vector (vis))
            clef_->set_property ("break-visibility", vis);
        }

      clef_ = 0;

      modifier_ = 0;
    }
}

void
Clef_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Clef_engraver, bar_line);
}

ADD_TRANSLATOR (Clef_engraver,
                /* doc */
                "Determine and set reference point for pitches.",

                /* create */
                "Clef "
                "ClefModifier ",

                /* read */
                "clefGlyph "
                "clefTransposition "
                "clefTranspositionStyle "
                "clefPosition "
                "explicitClefVisibility "
                "forceClef ",

                /* write */
                "");
