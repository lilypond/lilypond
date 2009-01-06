/*
  clef-engraver.cc -- implement Clef_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>,

  Mats Bengtsson <matsb@s3.kth.se>
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

#include "translator.icc"

class Clef_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Clef_engraver);

  Direction octave_dir_;

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
  void set_glyph ();
  void inspect_clef_properties ();
};

void
Clef_engraver::derived_mark () const
{
  scm_gc_mark (prev_octavation_);
  scm_gc_mark (prev_cpos_);
  scm_gc_mark (prev_glyph_);
}

Clef_engraver::Clef_engraver ()
{
  clef_ = 0;
  octave_dir_ = CENTER;
  octavate_ = 0;

  /*
    will trigger a clef at the start since #f != ' ()
  */
  prev_octavation_ = prev_cpos_ = prev_glyph_ = SCM_BOOL_F;
}

void
Clef_engraver::set_glyph ()
{
  SCM glyph_sym = ly_symbol2scm ("glyph");
  SCM basic = ly_symbol2scm ("Clef");

  execute_pushpop_property (context (), basic, glyph_sym, SCM_UNDEFINED);
  execute_pushpop_property (context (), basic, glyph_sym, get_property ("clefGlyph"));
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

      SCM oct = get_property ("clefOctavation");
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
}
void
Clef_engraver::process_music ()
{
  inspect_clef_properties ();
}

void
Clef_engraver::inspect_clef_properties ()
{
  SCM glyph = get_property ("clefGlyph");
  SCM clefpos = get_property ("clefPosition");
  SCM octavation = get_property ("clefOctavation");
  SCM force_clef = get_property ("forceClef");

  if (clefpos == SCM_EOL
      || scm_equal_p (glyph, prev_glyph_) == SCM_BOOL_F
      || scm_equal_p (clefpos, prev_cpos_) == SCM_BOOL_F
      || scm_equal_p (octavation, prev_octavation_) == SCM_BOOL_F
      || to_boolean (force_clef))
    {
      set_context_property_on_children (context (),
					ly_symbol2scm ("localKeySignature"),
					get_property ("keySignature"));
      
      set_glyph ();
      if (prev_cpos_ != SCM_BOOL_F || to_boolean (get_property ("firstClef")))
	create_clef ();

      if (clef_)
	clef_->set_property ("non-default", SCM_BOOL_T);

      prev_cpos_ = clefpos;
      prev_glyph_ = glyph;
      prev_octavation_ = octavation;
    }

  if (to_boolean (force_clef))
    {
      SCM prev;
      Context *w = context ()->where_defined (ly_symbol2scm ("forceClef"), &prev);
      w->set_property ("forceClef", SCM_EOL);
    }
}

void
Clef_engraver::stop_translation_timestep ()
{
  if (clef_)
    {
      SCM vis = 0;
      if (to_boolean (clef_->get_property ("non-default")))
	vis = get_property ("explicitClefVisibility");

      if (vis)
	{
	  clef_->set_property ("break-visibility", vis);
	  if (octavate_)
	    octavate_->set_property ("break-visibility", vis);
	}

      clef_ = 0;

      octavate_ = 0;
    }
}

ADD_ACKNOWLEDGER (Clef_engraver, bar_line);
ADD_TRANSLATOR (Clef_engraver,
		/* doc */
		"Determine and set reference point for pitches.",

		/* create */
		"Clef "
		"OctavateEight ",

		/* read */
		"clefGlyph "
		"clefOctavation "
		"clefPosition "
		"explicitClefVisibility "
		"forceClef ",

		/* write */
		""
		);
