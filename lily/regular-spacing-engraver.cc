/*   
  regular-spacing-engraver.cc --  implement Regular_spacing_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2001--2002  Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "grob.hh"

class Regular_spacing_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS(Regular_spacing_engraver);
  Moment last_moment_;
  SCM last_col_;
protected:
  virtual void process_music ();
};

Regular_spacing_engraver::Regular_spacing_engraver ()
{
  last_col_ = SCM_EOL;
}

void
Regular_spacing_engraver::process_music ()
{
  SCM delta = get_property ("regularSpacingDelta");

  if (unsmob_moment (delta))
    {
      SCM mp = get_property ("measurePosition");
      if (!unsmob_moment (mp))
	return;

      Rational d = unsmob_moment (delta)->main_part_;
      Rational p = unsmob_moment (mp)->main_part_;

      if (p.mod_rat (d) != Rational (0))
	return;

      Moment now = now_mom ();
      SCM col = get_property ("currentMusicalColumn");
      if (p
	  && (now -last_moment_ ).main_part_ == d)
	{
	  unsmob_grob (col)->set_grob_property ("regular-distance-to", last_col_);
	}
      last_col_ = col;
      last_moment_ = now;
    }
}


ENTER_DESCRIPTION (Regular_spacing_engraver,
/* descr */       ".",
/* creats*/       "",
/* acks  */       "",
/* reads */       "regularSpacingDelta",
/* write */       "regular-distance-to");

