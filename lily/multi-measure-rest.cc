/*   
  multi-measure-rest.cc --  implement Multi_measure_rest
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#include "multi-measure-rest.hh"
#include "debug.hh"
#include "paper-def.hh"
#include "paper-column.hh" // urg
#include "lookup.hh"
#include "rest.hh"
#include "molecule.hh"
#include "misc.hh"
#include "group-interface.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "text-item.hh"

void
Multi_measure_rest::set_interface (Score_element*me)
{
  me->set_interface (ly_symbol2scm ("multi-measure-rest-interface"));
}

bool
Multi_measure_rest::has_interface (Score_element*me)
{
  return me->has_interface (ly_symbol2scm ("multi-measure-rest-interface"));
}

  /*
   [TODO]                                      17
 * variable-sized multi-measure rest symbol: |====| ??
*/
MAKE_SCHEME_CALLBACK(Multi_measure_rest,brew_molecule,1);
SCM
Multi_measure_rest::brew_molecule (SCM smob) 
{
  Score_element *me = unsmob_element (smob);
  Spanner * sp = dynamic_cast<Spanner*> (me);
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  Interval sp_iv;
  Direction d = LEFT;
  do
    {
      Item * col = sp->get_bound (d)->column_l ();

      Interval coldim = col->extent (0, X_AXIS);

      sp_iv[d] = coldim[-d]  ;
    }
  while ((flip (&d)) != LEFT);
  Molecule mol;
  Real x_off = 0.0;

  Real rx  = sp->get_bound (LEFT)->relative_coordinate (0, X_AXIS);
  /*
    we gotta stay clear of sp_iv, so move a bit to the right if
    needed.
   */
  x_off += (sp_iv[LEFT] -  rx) >? 0;

  /*
    center between stuff.
   */
  x_off += sp_iv.length ()/ 2;

  
  Molecule s;

  int measures = 1;
  SCM m (me->get_elt_property ("measure-count"));
  if (gh_number_p (m))
    {
      measures = gh_scm2int (m);
    }
  

  SCM limit = me->get_elt_property ("expand-limit");
  if (measures <= gh_scm2int (limit))
    {
      /*
	Build a rest from smaller parts. Distances inbetween are
	really variable, see Wanske pp. 125 */

      int l = measures;
      while (l)
	{
	  int k;
	  if (l >= 4)
	    {
	      l-=4;
	      k = -2;
	    }
	  else if (l>= 2)
	    {
	      l -= 2;
	      k = -1;
	    }
	  else
	    {
	      k = 0;
	      l --;
	    }

	  Real pad = s.empty_b ()
	    ? 0.0 : gh_scm2double (me->get_elt_property ("padding")) * staff_space;

	  Molecule r (me->lookup_l ()->afm_find ("rests-" + to_str (k)));
	  if (k == 0)
	    r.translate_axis (staff_space, Y_AXIS);
	  
	  s.add_at_edge (X_AXIS, RIGHT, r, pad);
	}


      s.align_to (X_AXIS, CENTER);
    }
  else 
    {
      String idx =  ("rests-") + to_str (-4);
      s = me->lookup_l ()->afm_find (idx);
    }
  
  mol.add_molecule (s);

  if (measures > 1)
    {
      SCM properties = gh_append2 (me->immutable_property_alist_,
				   me->mutable_property_alist_);
      Molecule s =
	Text_item::text2molecule (me,
				  ly_str02scm (to_str (measures).ch_C ()),
				  properties);
      s.align_to (X_AXIS, CENTER);
      s.translate_axis (3.0 * staff_space, Y_AXIS);
      mol.add_molecule (s);
    }
  mol.translate_axis (x_off, X_AXIS);
  return mol.create_scheme();
}

/*
  UGH. JUNKME elt prop "columns" isn't really needed. 
 */
void
Multi_measure_rest::add_column (Score_element*me,Item* c)
{
  Pointer_group_interface::add_element (me, "columns",c);

  add_bound_item (dynamic_cast<Spanner*> (me),c);
}


MAKE_SCHEME_CALLBACK (Multi_measure_rest, set_spacing_rods,1);

SCM
Multi_measure_rest::set_spacing_rods (SCM smob)
{
  Score_element*me = unsmob_element (smob);

  Spanner*sp = dynamic_cast<Spanner*> (me);
  if (!(sp->get_bound (LEFT) && sp->get_bound (RIGHT)))
    {
      programming_error ("Multi_measure_rest::get_rods (): I am not spanned!");
      return SCM_UNSPECIFIED;
    }

  Item * l = sp->get_bound (LEFT)->column_l ();
  Item * r = sp->get_bound (RIGHT)->column_l ();
  Item * lb = l->find_prebroken_piece (RIGHT);
  Item * rb = r->find_prebroken_piece (LEFT);      
  
  Item* combinations[4][2]={{l,r}, {lb,r}, {l,rb},{lb,rb}};
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  for (int i=0; i < 4; i++)
    {
      Item * l =  combinations[i][0];
      Item *r = combinations[i][1];

      if (!l || !r)
	continue;

      Rod rod;
      rod.item_l_drul_[LEFT] = l;
      rod.item_l_drul_[RIGHT] = r;

	/*
	  should do something more advanced.
	 */
      rod.distance_f_ = l->extent (l, X_AXIS)[BIGGER] - r->extent (r, X_AXIS)[SMALLER]
	+ gh_scm2double (me->get_elt_property ("minimum-width")) * staff_space;
  
      rod.add_to_cols ();
    }
  return SCM_UNSPECIFIED;
}

