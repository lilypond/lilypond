/*
  break-align-item.cc -- implement Break_align_interface

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include <math.h>
#include <libc-extension.hh>

#include "side-position-interface.hh"
#include "axis-group-interface.hh"
#include "warn.hh"
#include "lily-guile.hh"
#include "break-align-item.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "paper-column.hh"
#include "group-interface.hh"
#include "align-interface.hh"

MAKE_SCHEME_CALLBACK(Break_align_interface,before_line_breaking,1);

SCM
Break_align_interface::before_line_breaking (SCM smob)
{
  Grob* me = unsmob_element (smob);
  do_alignment (me);
  return SCM_UNSPECIFIED;
}
MAKE_SCHEME_CALLBACK(Break_align_interface,alignment_callback,2);

SCM
Break_align_interface::alignment_callback (SCM element_smob, SCM axis)
{
  Grob *me = unsmob_element (element_smob);
  Axis a = (Axis) gh_scm2int (axis);

  assert (a == X_AXIS);
  Grob *par = me->parent_l (a);
  if (par && !to_boolean (par->get_grob_property ("break-alignment-done")))\
    {
      par->set_grob_property ("break-alignment-done", SCM_BOOL_T);
      Break_align_interface::do_alignment (par);
    }
    
  return gh_double2scm (0);
}

MAKE_SCHEME_CALLBACK(Break_align_interface,self_align_callback,2);
SCM
Break_align_interface::self_align_callback (SCM element_smob, SCM axis)
{
  Grob *me = unsmob_element (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  assert (a == X_AXIS);
  
  Item* item = dynamic_cast<Item*> (me);
  Direction bsd = item->break_status_dir();
  if (bsd == LEFT)
    {
      me->set_grob_property ("self-alignment-X", gh_int2scm (RIGHT));
    }

  return Side_position::aligned_on_self (element_smob, axis);  
}

void
Break_align_interface::add_element (Grob*me, Grob *toadd)
{
  Axis_group_interface::add_element (me, toadd);
}

void
Break_align_interface::do_alignment (Grob *me)
{
  Item * item = dynamic_cast<Item*> (me);
  Item *column = item->column_l ();

  Link_array<Grob> elems;
  Link_array<Grob> all_elems
    = Pointer_group_interface__extract_elements (me, (Grob*)0,
						 "elements");
  
  for (int i=0; i < all_elems.size(); i++) 
    {
      Interval y = all_elems[i]->extent(all_elems[i], X_AXIS);
      if (!y.empty_b())
	elems.push (dynamic_cast<Grob*> (all_elems[i]));
    }
  
  if (!elems.size ())
    return;

  SCM symbol_list = SCM_EOL;
  Array<Real> dists;
  SCM current_origin = ly_symbol2scm ("none");
  for (int i=0; i <= elems.size (); i++)
    {
      Grob *next_elt  = i < elems.size ()
	? elems[i]
	: 0 ;
      
      SCM next_origin;

      if (next_elt)
	{
	  next_origin = next_elt->get_grob_property ("break-align-symbol");
	  next_origin =
	    gh_symbol_p (next_origin)? 
	    next_origin : ly_symbol2scm ("none")
;
	}
      else
	next_origin = ly_symbol2scm ("begin-of-note");

      SCM alist = me->get_grob_property ("space-alist");
      SCM e = scm_assoc (scm_listify (current_origin,
				      next_origin,
				      SCM_UNDEFINED), alist);
          
      SCM extra_space;
      if (e != SCM_BOOL_F)
	{
	  extra_space = gh_cdr (e);
	}
      else
	{
	  warning (_f ("unknown spacing pair `%s', `%s'",
		       ly_symbol2string (current_origin),
		       ly_symbol2string (next_origin)));
	  extra_space = scm_listify (ly_symbol2scm ("minimum-space"), gh_double2scm (0.0), SCM_UNDEFINED);
	}

      SCM symbol = gh_car  (extra_space);
      Real spc = gh_scm2double (gh_cadr(extra_space));

      dists.push(spc);
      symbol_list = gh_cons (symbol, symbol_list);
      current_origin = next_origin;
    }


  // skip the first sym.
  symbol_list  = gh_cdr (scm_reverse (symbol_list));
  for (int i=0; i <elems.size()-1; i++)
    {
      elems[i]->set_grob_property (gh_car  (symbol_list),
				  scm_cons (gh_double2scm (0),
					    gh_double2scm (dists[i+1])));

      symbol_list = gh_cdr (symbol_list);
    }


  // urg
  SCM first_pair = elems[0]->get_grob_property ("minimum-space");
  if (gh_pair_p (first_pair))
    first_pair = first_pair;
  else
    first_pair = gh_cons (gh_double2scm (0.0), gh_double2scm (0.0));
  
  scm_set_car_x (first_pair, gh_double2scm (-dists[0]));
  elems[0]->set_grob_property ("minimum-space", first_pair);


  /*
    Force callbacks for alignment to be called   
  */
  Align_interface::do_side_processing (me, X_AXIS);

  Real pre_space = elems[0]->relative_coordinate (column, X_AXIS);

  Real xl = elems[0]->extent (elems[0],X_AXIS)[LEFT];
  if (!isinf (xl))
    pre_space += xl;
  else
    programming_error ("Infinity reached. ");

  Real xr = elems.top ()->extent (elems.top (), X_AXIS)[RIGHT];
  Real spring_len = elems.top ()->relative_coordinate (column, X_AXIS);
  if (!isinf (xr))
    spring_len += xr;
  else
    programming_error ("Infinity reached.");
  
  Real stretch_distance =0.;
  
  if (gh_car  (symbol_list) == ly_symbol2scm ("extra-space"))
    {
      spring_len += dists.top ();
      stretch_distance = dists.top ();
    }
  else if (gh_car  (symbol_list) == ly_symbol2scm ("minimum-space"))
    {
      spring_len = spring_len >? dists.top ();
      stretch_distance = spring_len;
    }

  /*
    Hint the spacing engine how much space to put in.

    The pairs are in the format of an interval (ie. CAR <  CDR).
  */
  column->set_grob_property ("extra-space",
			    scm_cons (gh_double2scm (pre_space),
				      gh_double2scm (spring_len)));

  column->set_grob_property ("stretch-distance",
			    gh_cons (gh_double2scm (-dists[0]),
				     gh_double2scm (stretch_distance)));
}


void
Break_align_interface::set_interface (Grob*me)
{
  Align_interface::set_interface (me); 
  Align_interface::set_axis (me,X_AXIS);


}
