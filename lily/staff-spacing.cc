/*   
staff-spacing.cc --  implement Staff_spacing

source file of the GNU LilyPond music typesetter

(c) 2001--2002  Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */
#include "paper-column.hh" 
#include "separation-item.hh"
#include "item.hh"
#include "staff-spacing.hh"
#include "grob.hh"
#include "warn.hh"

bool
Staff_spacing::has_interface (Grob* g)
{
  return g && g->has_interface (ly_symbol2scm ("staff-spacing-interface"));
}


/*
*/
void
Staff_spacing::get_spacing_params (Grob *me, Real * space, Real * fixed)
{
  *space = 1.0;
  *fixed = 1.0;

  

  Grob * separation_item=0;
  
  for (SCM s = me->get_grob_property ("left-items");
       gh_pair_p (s); s = gh_cdr(s))
    {
      Grob * cand = unsmob_grob(gh_car (s));
      if (cand && Separation_item::has_interface (cand))
	separation_item = cand ;
    }

  Grob *left_col = dynamic_cast<Item*> (me)->column_l ();

  Grob *last_grob = 0;
  Interval last_ext ;

  if (!separation_item)
    {
      programming_error ("no sep item");
      return;
    }
  
  for (SCM s = separation_item->get_grob_property ("elements");
       gh_pair_p (s); s = gh_cdr (s))
    {
      Grob * break_item = unsmob_grob (gh_car (s));

      
      if (!gh_symbol_p (break_item->get_grob_property ("break-align-symbol")))
	continue;

      Interval ext = break_item->extent (left_col, X_AXIS);

      if (ext.empty_b ())
	continue;
      if (!last_grob
	  || (last_grob && ext[RIGHT] > last_ext[RIGHT]))
	{
	  last_ext = ext;
	  last_grob = break_item; 
	}
    }

  if (!last_grob)
    {
      programming_error ("empty break column? --fixme");
      return ;
    }

  *fixed = last_ext[RIGHT];
  *space = *fixed + 1.0;
  
  SCM alist = last_grob->get_grob_property ("space-alist");
  if (!scm_list_p (alist))
    return ;

  SCM space_def = scm_sloppy_assq (ly_symbol2scm ("begin-of-note"), alist);
  if (!gh_pair_p (space_def))
    {
      programming_error ("Unknown prefatory spacing. "); 
      return; 
    }

  space_def = gh_cdr (space_def);
  Real distance = gh_scm2double (gh_cdr (space_def));
  SCM type = gh_car (space_def) ;

  *fixed = last_ext[RIGHT];
  if (type == ly_symbol2scm ("extra-space"))
    *space = *fixed + distance;
  else if (type == ly_symbol2scm("minimum-space"))
    *space = last_ext[LEFT] + (last_ext.length () >? distance);
  
}
