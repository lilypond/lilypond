/*   
  piano-pedal-bracket.cc --  implement  Piano_pedal_bracket

source file of the GNU LilyPond music typesetter

(c) 2003 Han-Wen Nienhuys <hanwen@xs4all.nl>

based on smouldering remains by

   Chris Jackson <chris@fluffhouse.org.uk>


*/
/* 
   Piano pedal brackets are a special case of a text spanner.
   Pedal up-down (restart) indicated by the angled right and left edges 
   of consecutive pedals touching exactly to form an __/\__

*/


/*
  TODO: this should be moved somewhere else (?).

  Perhaps make separate function for pedal-bracket.
 */
#include "molecule.hh"
#include "spanner.hh"
#include "item.hh"
#include "paper-def.hh"

struct Piano_pedal_bracket
{
  DECLARE_SCHEME_CALLBACK(after_line_breaking,(SCM));
  static bool has_interface (Grob*);
};

ADD_INTERFACE (Piano_pedal_bracket,"piano-pedal-bracket-interface",
	       "",
	       "pedal-text");

MAKE_SCHEME_CALLBACK(Piano_pedal_bracket,after_line_breaking,1);
SCM
Piano_pedal_bracket::after_line_breaking (SCM smob)
{
  Spanner *me = dynamic_cast<Spanner*> (unsmob_grob (smob));

  Drul_array<bool> broken;
  Drul_array<Real> height(0,0), shorten(0,0);

  SCM eh = me->get_grob_property ("edge-height");
  SCM sp = me->get_grob_property ("shorten-pair");
  
  Direction d = LEFT;

  do
    {
      Item *b = me->get_bound (d);
      broken[d] = b->break_status_dir () != CENTER;

      if (!broken[d] && (ly_number_pair_p (eh)))
	height[d] += gh_scm2double (index_get_cell (eh, d));
      if (ly_number_pair_p (sp))
	shorten[d] +=  gh_scm2double (index_get_cell (sp, d));
    }
  while (flip (&d) != LEFT);
  
  /* For 'Mixed' style pedals, i.e.  a bracket preceded by text:  Ped._____|
   need to shorten by the extent of the text grob


   Urg. - why not hang bracket between text items? --hwn
  */
  if (Grob *textbit = unsmob_grob (me->get_grob_property("pedal-text")))
    {
      height[LEFT] = 0;
      SCM pa = me->get_grob_property ("if-text-padding"); // UGH.
      Real padding =0.;
      if (gh_number_p (pa))
	padding = gh_scm2double (pa);
	  
      shorten[LEFT] += padding + textbit->extent (textbit, X_AXIS)[RIGHT];
    }
  
  if (broken[LEFT])
    {
      shorten[LEFT]  -=  me->get_broken_left_end_align () ;
    }

  // Also shorten so that it ends just before the spanned note.
  Grob  *rb = me->get_bound (RIGHT);
  shorten[RIGHT]  += rb->extent (rb, X_AXIS)[RIGHT];
    
  me->set_grob_property ("edge-height", ly_interval2scm (height));
  me->set_grob_property ("shorten-pair", ly_interval2scm (shorten));

  return SCM_UNSPECIFIED;
}

