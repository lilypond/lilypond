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
  
  for (int i = 0; i < text.length (); i++) 
    {
      
      switch (text[i]) 
	{
	case '\\':
  // accent marks use width of base letter
         if (i +1 < text.length ())
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
	  
	  for (i++; (i < text.length ()) && !isspace (text[i]) 
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
  self_scm_ = SCM_EOL;
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


void
Font_metric::derived_mark ()const
{
  
}

  

SCM
Font_metric::mark_smob (SCM s)
{
  Font_metric * m = (Font_metric*) SCM_CELL_WORD_1 (s);

  m->derived_mark();
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


LY_DEFINE(ly_find_glyph_by_name, "ly-find-glyph-by-name", 2 , 0, 0,
	  (SCM font, SCM name),
	  "This function retrieves a Molecule for the glyph named @var{name} in
@var{font}.  The font must be available as an AFM file.")
{
  Font_metric *fm = unsmob_metrics (font);
  SCM_ASSERT_TYPE(fm, font, SCM_ARG1, __FUNCTION__, "font-metric");
  SCM_ASSERT_TYPE(gh_string_p (name), name, SCM_ARG2, __FUNCTION__, "string");

  return fm->find_by_name (ly_scm2string (name)).smobbed_copy ();
}


LY_DEFINE(ly_text_dimension,"ly-text-dimension", 2 , 0, 0,
	  (SCM font, SCM text),
	  "Given the font metric in @var{font} and the string @var{text}, compute
the extents of that text in that font. The return value is a pair of
number-pairs.")
{
  Box b;
  Font_metric *fm = unsmob_metrics (font);
  SCM_ASSERT_TYPE(fm, font, SCM_ARG1, __FUNCTION__, "font-metric");
  SCM_ASSERT_TYPE(gh_string_p (text), text, SCM_ARG2, __FUNCTION__, "string");

  b = fm->text_dimension (ly_scm2string (text));
  
  return gh_cons (ly_interval2scm (b[X_AXIS]), ly_interval2scm(b[Y_AXIS]));
}




  
Molecule
Font_metric::get_char_molecule (int code)  const
{
  Molecule  m ;
  SCM at = scm_list_n (ly_symbol2scm ("char"), gh_int2scm (code),
		       SCM_UNDEFINED);
  at = fontify_atom (this, at);
  Box b = get_char (code);
  return Molecule (b, at);
}
