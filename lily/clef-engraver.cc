/*
  clef-engraver.cc -- implement Clef_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>,

  Mats Bengtsson <matsb@s3.kth.se>
*/

#include <ctype.h>

#include "translator-group.hh"

#include "bar-line.hh"
#include "staff-symbol-referencer.hh"
#include "debug.hh"
#include "engraver.hh"
#include "direction.hh"
#include "side-position-interface.hh"
#include "item.hh"


class Clef_engraver : public  Engraver
{
public:
  TRANSLATOR_DECLARATIONS(Clef_engraver);

  Direction octave_dir_;

protected:
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void process_music ();
  virtual void acknowledge_grob (Grob_info);
private:
  Item * clef_p_;
  Item * octavate_p_;

  SCM prev_glyph_;
  SCM prev_cpos_;
  SCM prev_octavation_;
  void create_clef ();
  void set_glyph ();
  void inspect_clef_properties ();
};

Clef_engraver::Clef_engraver ()
{
  clef_p_ = 0;
  octave_dir_ = CENTER;
  octavate_p_ = 0;

  /*
    will trigger a clef at the start since #f != ' ()
   */
  prev_cpos_ = prev_glyph_ = SCM_BOOL_F;
}

void
Clef_engraver::set_glyph ()
{
  SCM glyph_sym = ly_symbol2scm ("glyph-name");
  SCM glyph = get_property ("clefGlyph");

  SCM basic = ly_symbol2scm ("Clef");
  
  daddy_trans_l_->execute_single_pushpop_property (basic, glyph_sym, SCM_UNDEFINED);
  daddy_trans_l_->execute_single_pushpop_property (basic, glyph_sym, glyph);
}

/** 
  Generate a clef at the start of a measure. (when you see a Bar,
  ie. a breakpoint) 
  */
void
Clef_engraver::acknowledge_grob (Grob_info info)
{
  Item * item =dynamic_cast <Item *> (info.grob_l_);
  if (item)
    {
      if (Bar_line::has_interface (info.grob_l_)
	  && gh_string_p (get_property ("clefGlyph")))
	create_clef ();

    } 
}

void
Clef_engraver::create_clef ()
{
  if (!clef_p_)
    {
      Item *c= new Item (get_property ("Clef"));
      announce_grob(c, SCM_EOL);

      clef_p_ = c;
    }
  SCM cpos = get_property ("clefPosition");

  if (gh_number_p (cpos))
    Staff_symbol_referencer::set_position (clef_p_, gh_scm2int (cpos));

  SCM oct =  get_property ("clefOctavation");
  if (gh_number_p (oct) && gh_scm2int (oct))
    {
      Item * g = new Item (get_property ("OctavateEight"));

      Side_position_interface::add_support (g,clef_p_);      

      g->set_parent (clef_p_, Y_AXIS);
      g->set_parent (clef_p_, X_AXIS);

      g->set_grob_property ("direction", gh_int2scm (sign (gh_scm2int (oct))));
      octavate_p_ = g;
      announce_grob(octavate_p_, SCM_EOL);
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
      || to_boolean (force_clef)
)
    {
      set_glyph ();
      create_clef ();

      clef_p_->set_grob_property ("non-default", SCM_BOOL_T);

      prev_cpos_ = clefpos;
      prev_glyph_ = glyph;
      prev_octavation_ = octavation;
    }

  if (to_boolean (force_clef))
    {
      Translator_group * w = daddy_trans_l_->where_defined (ly_symbol2scm ("forceClef"));
      w->set_property ("forceClef", SCM_EOL);
    }
}


void
Clef_engraver::stop_translation_timestep ()
{
  if (clef_p_)
    {
      SCM vis = 0; 
      if (to_boolean (clef_p_->get_grob_property ("non-default")))
	{
	  vis = get_property ("explicitClefVisibility");
	}

      if (vis)
	{
	  clef_p_->set_grob_property ("visibility-lambda", vis);
	  if (octavate_p_)
	    octavate_p_->set_grob_property ("visibility-lambda", vis);
	}
      
      typeset_grob (clef_p_);
      clef_p_ =0;

      if (octavate_p_)
	typeset_grob (octavate_p_);

      octavate_p_ = 0;
    }
}

void
Clef_engraver::start_translation_timestep ()
{
}



ENTER_DESCRIPTION(Clef_engraver,
/* descr */       "Determine and set reference point for pitches",
/* creats*/       "Clef OctavateEight",
/* acks  */       "bar-line-interface",
/* reads */       "clefPosition clefGlyph centralCPosition clefOctavation explicitClefVisibility",
/* write */       "");
