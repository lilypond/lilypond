/*   
  font-metric.cc --  implement Font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>

    Mats Bengtsson <matsb@s3.kth.se>  (the ugly TeX parsing in text_dimension)
 */

#include <math.h>
#include <ctype.h>

#include "molecule.hh"
#include "ly-smobs.icc"
#include "font-metric.hh"
#include "string.hh"

Box
Font_metric::text_dimension (String text) const
{
  Interval ydims;
  Real w=0.0;
  
  for (int i = 0; i < text.length_i (); i++) 
    {
      
      switch (text[i]) 
	{
	case '\\':
	  for (i++; (i < text.length_i ()) && !isspace(text[i]) 
		 && text[i]!='{' && text[i]!='}'; i++)
	    ;
	  // ugh.
	  i--; // Compensate for the increment in the outer loop!
	  break;
	case '{':  // Skip '{' and '}'
	case '}':
	  break;
	
	default: 
	  Box b = get_char ((unsigned char)text[i]);
	  
	  // Ugh, use the width of 'x' for unknown characters
	  if (b[X_AXIS].length () == 0) 
	    b = get_char ((unsigned char)'x');
	  
	  w += b[X_AXIS].length ();
	  ydims.unite (b[Y_AXIS]);
	  break;
	}
    }
  if (ydims.empty_b ())
    ydims = Interval (0,0);

  return Box(Interval (0, w), ydims);
}


Box
Scaled_font_metric::text_dimension (String t) const
{
  Box b (orig_l_->text_dimension (t));

  b.scale (magnification_f_);
  return b;
}

Font_metric::~Font_metric ()
{
}

Font_metric::Font_metric ()
{
  description_ = SCM_EOL;

  smobify_self ();
}

Font_metric::Font_metric (Font_metric const &)
{
}


Box 
Font_metric::get_char (int )const
{
  return Box (Interval(0,0),Interval (0,0));
}


SCM
Font_metric::mark_smob (SCM s)
{
  Font_metric * m = (Font_metric*) SCM_CELL_WORD_1(s);
  return m->description_;
}

int
Font_metric::print_smob (SCM s, SCM port, scm_print_state * )
{
  Font_metric *m = unsmob_metrics (s);
  scm_puts ("#<Font_metric ", port);
  scm_write (m->description_, port);
  scm_puts (">", port);
  return 1;
}


IMPLEMENT_UNSMOB (Font_metric, metrics);
IMPLEMENT_SMOBS (Font_metric);
IMPLEMENT_DEFAULT_EQUAL_P(Font_metric);
IMPLEMENT_TYPE_P (Font_metric, "font-metric?");

Molecule
Font_metric::find_by_name (String) const
{
  Molecule m ;
  return m;
}



