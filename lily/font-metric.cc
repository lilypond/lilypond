/*   
  font-metric.cc --  implement Font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include <math.h>
#include <ctype.h>


#include "font-metric.hh"

Box
Font_metric::text_dimension (String text) const
{
  Interval ydims;
  Real w=0.0;
  
  for (int i = 0; i < text.length_i (); i++) 
    {
      
      if (text[i]=='\\') 
	{
	  for (i++; (i < text.length_i ()) && isalpha(text[i]); i++)
	    ;
	  // ugh.
	  i--; // Compensate for the increment in the outer loop!
	}
      else
	{
	  Character_metric const *c = get_char ((unsigned char)text[i],false);

	  // Ugh, use the width of 'x' for unknown characters
	  if (c->dimensions()[X_AXIS].length () == 0) 
	    c = get_char ((unsigned char)'x',false);
	  
	  w += c->dimensions()[X_AXIS].length ();
	  ydims.unite (c->dimensions()[Y_AXIS]);
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


Box
Character_metric::dimensions () const
{
  return Box(Interval(0,0), Interval(0,0));
}

Font_metric::~Font_metric ()
{
  unsmobify_self ();
}

Font_metric::Font_metric ()
{
  self_scm_ = SCM_EOL;
  smobify_self ();
}

Font_metric::Font_metric (Font_metric const &)
{
}

Character_metric::~Character_metric()
{
}

Character_metric const *
Font_metric::get_char (int, bool)const
{
  return 0;
}

Scaled_font_metric::Scaled_font_metric (Font_metric* m, int s)
{
  magstep_i_ = s;
  orig_l_ = m;
}

SCM
Font_metric::description () const
{
  return gh_cons (ly_symbol2scm (name_str_.ch_C()), gh_int2scm (0));
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
  return SCM_EOL;
}

int
Font_metric::print_smob (SCM s, SCM port, scm_print_state * )
{
  scm_puts ("#<Font_metric>", port);
  
  return 1;
}




