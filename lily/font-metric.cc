/*   
  font-metric.cc --  implement Font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>

    Mats Bengtsson <matsb@s3.kth.se> (the ugly TeX parsing in text_dimension)
 */

#include <math.h>
#include <ctype.h>

#include "warn.hh"
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
  // accent marks use width of base letter
         if (i +1 < text.length_i ())
	   {
	     if (text[i+1]=='\'' || text[i+1]=='`' || text[i+1]=='"' ||
		 text[i+1]=='^')
	       {
		 i++;
		 break;
	       }
	     // for string width \\ is a \ and \_ is a _.
	     if (text[i+1]=='\\' || text[i+1]=='_')        
	       {
		 break;
	       }
	   }
	  
	  for (i++; (i < text.length_i ()) && !isspace (text[i]) 
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

  return Box (Interval (0, w), ydims);
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

int
Font_metric::count () const
{
  return 0;
}

Box 
Font_metric::get_char (int)const
{
  return Box (Interval (0,0),Interval (0,0));
}


SCM
Font_metric::mark_smob (SCM s)
{
  Font_metric * m = (Font_metric*) SCM_CELL_WORD_1 (s);
  return m->description_;
}

int
Font_metric::print_smob (SCM s, SCM port, scm_print_state *)
{
  Font_metric *m = unsmob_metrics (s);
  scm_puts ("#<Font_metric ", port);
  scm_write (m->description_, port);
  scm_puts (">", port);
  return 1;
}



IMPLEMENT_SMOBS (Font_metric);
IMPLEMENT_DEFAULT_EQUAL_P (Font_metric);
IMPLEMENT_TYPE_P (Font_metric, "font-metric?");

Molecule
Font_metric::find_by_name (String) const
{
  Molecule m ;
  return m;
}


SCM
ly_find_glyph_by_name (SCM font, SCM name)
{
  if (!unsmob_metrics (font) || !gh_string_p (name))
    {
      warning ("ly-find-glyph-by-name: invalid argument.");
      Molecule m;
      return m.smobbed_copy ();
    }

  return unsmob_metrics (font)->find_by_name (ly_scm2string (name)).smobbed_copy ();
}


SCM
ly_text_dimension (SCM font, SCM text)
{
  Box b;
  
  if (!unsmob_metrics (font) || !gh_string_p(text))
    {
      warning ("ly-find-glyph-by-name: invalid argument.");
      Molecule m;
      return m.smobbed_copy ();
    }
  else
    {
      b = unsmob_metrics (font)->text_dimension (ly_scm2string (text));
    }
  
  return gh_cons (ly_interval2scm (b[X_AXIS]), ly_interval2scm(b[Y_AXIS]));
}


static void
font_metric_init ()
{
   scm_c_define_gsubr ("ly-find-glyph-by-name", 2 , 0, 0,
		       (Scheme_function_unknown) ly_find_glyph_by_name);
   scm_c_define_gsubr ("ly-text-dimension", 2 , 0, 0,
		       (Scheme_function_unknown) ly_text_dimension);
}

ADD_SCM_INIT_FUNC (font_metric_init, font_metric_init);
