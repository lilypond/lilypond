/*   
  font-metric.cc --  implement Font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>

    Mats Bengtsson <matsb@s3.kth.se>  (the ugly TeX parsing in text_dimension)
 */

#include <math.h>
#include <ctype.h>

#include "ly-smobs.icc"
#include "font-metric.hh"

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
	  Box b = get_char ((unsigned char)text[i],false);
	  
	  // Ugh, use the width of 'x' for unknown characters
	  if (b[X_AXIS].length () == 0) 
	    b = get_char ((unsigned char)'x',false);
	  
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
  Real realmag = pow (1.2, magstep_i_);
  Box b (orig_l_->text_dimension (t));

  return Box(b[X_AXIS]* realmag, b[Y_AXIS]*realmag);
}

Font_metric::~Font_metric ()
{
  unsmobify_self ();
}

Font_metric::Font_metric ()
{
  self_scm_ = SCM_EOL;
  name_ = SCM_EOL;
  smobify_self ();
}

Font_metric::Font_metric (Font_metric const &)
{
}


Box 
Font_metric::get_char (int, bool)const
{
  return Box (Interval(0,0),Interval (0,0));
}

Scaled_font_metric::Scaled_font_metric (Font_metric* m, int s)
{
  magstep_i_ = s;
  orig_l_ = m;
}

SCM
Font_metric::description () const
{
  return gh_cons (name_, gh_int2scm (0));
}


SCM
Scaled_font_metric::description () const
{
  SCM od = orig_l_->description ();
  gh_set_cdr_x (od, gh_int2scm (magstep_i_));
  return od;
}


void
Font_metric::do_smobify_self ()
{
}

SCM
Font_metric::mark_smob (SCM s)
{
  Font_metric * m = SMOB_TO_TYPE(Font_metric, s);
  return m->name_;
}

int
Font_metric::print_smob (SCM s, SCM port, scm_print_state * )
{
  Font_metric *m = unsmob_metrics (s);
  scm_puts ("#<Font_metric ", port);
  scm_display (m->name_, port);
  scm_puts (">", port);
  return 1;
}


IMPLEMENT_UNSMOB (Font_metric, metrics);
IMPLEMENT_SMOBS (Font_metric);

