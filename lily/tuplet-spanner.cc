/*
  plet-spanner.cc -- implement Tuplet_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "beam.hh"
#include "box.hh"
#include "debug.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "tuplet-spanner.hh"
#include "stem.hh"
#include "note-column.hh"
#include "dimensions.hh"
#include "group-interface.hh"



Tuplet_spanner::Tuplet_spanner ()
{
  /*
    -> GUILE
   */
  parallel_beam_b_ = false;
  set_elt_property ("beams", SCM_EOL);
  set_elt_property ("columns", SCM_EOL);  
}

/*
  TODO. 
 */
Molecule*
Tuplet_spanner::do_brew_molecule_p () const
{
  Molecule* mol_p = new Molecule;

  // Default behaviour: number always, bracket when no beam!
  bool bracket_visibility = !parallel_beam_b_;
  bool number_visibility = true;
  SCM visibility_sym =get_elt_property ("tuplet-visibility");
  if (visibility_sym != SCM_UNDEFINED)
    {
      /*
	ARG. Fixme.
       */
      
      /* Property values:
	 0       show nothing
	 1       show number
	 2       show (number and bracket)-if-no-beam
	 3       show number, and bracket-if-no-beam
	 4       show number, and bracket
      */
      int value = gh_scm2int ((visibility_sym));
      bracket_visibility = (value == 4 || (value > 1 && !parallel_beam_b_));
      number_visibility = (value > 2 || value == 1 || 
			   (value == 2 && !parallel_beam_b_));
    }
  
  if (gh_pair_p (get_elt_property ("columns")))
    {
      Link_array<Note_column> column_arr=
	Group_interface__extract_elements (this, (Note_column*)0, "columns");
	
      Real ncw = column_arr.top ()->extent(X_AXIS).length ();
      Real w = spanner_length () + ncw;
      Molecule num (lookup_l ()->text ("italic",
				       number_str_, paper_l ()));
      num.align_to (X_AXIS, CENTER);
      num.translate_axis (w/2, X_AXIS);
      Real interline = paper_l ()->get_var ("interline");
      Real dy = column_arr.top ()->extent (Y_AXIS) [get_direction ()]
	- column_arr[0]->extent (Y_AXIS) [get_direction ()];
      num.align_to (Y_AXIS, CENTER);
      num.translate_axis (get_direction () * interline, Y_AXIS);
	
      num.translate_axis (dy/2, Y_AXIS);
    
      Real thick = paper_l ()->get_var ("tuplet_thick");
      if (bracket_visibility)      
	{
	  Real gap = paper_l () -> get_var ("tuplet_spanner_gap");
	
	  mol_p->add_molecule (lookup_l ()->tuplet_bracket (dy, w, thick, gap, interline, get_direction ()));
	}

      if (number_visibility)
	{
	  mol_p->add_molecule (num);
	}
      mol_p->translate_axis (get_direction () * interline, Y_AXIS);
    }
  return mol_p;
}
  
void
Tuplet_spanner::do_add_processing ()
{
  if (gh_pair_p (get_elt_property ("columns")))
    {
      Link_array<Note_column> column_arr=
	Group_interface__extract_elements (this, (Note_column*)0, "columns");
      
      set_bounds (LEFT, column_arr[0]);
      set_bounds (RIGHT, column_arr.top ());  
    }
}
  
void
Tuplet_spanner::do_post_processing ()
{
  Link_array<Note_column> column_arr=
    Group_interface__extract_elements (this, (Note_column*)0, "columns");
      

  if (column_arr.size())
    translate_axis (column_arr[0]->extent (Y_AXIS)[get_direction ()], Y_AXIS);

  
  if (scm_ilength (get_elt_property ("beams")) == 1)
    {
      SCM bs = get_elt_property ("beams");
      Score_element *b = unsmob_element (gh_car (bs));
      Beam * beam_l = dynamic_cast<Beam*> (b);
      if (!broken_b () 
	  && spanned_drul_[LEFT]->column_l () == beam_l->spanned_drul_[LEFT]->column_l ()
	  && spanned_drul_[RIGHT]->column_l () == beam_l->spanned_drul_[RIGHT]->column_l ())
	parallel_beam_b_ = true;
    }
}


Direction
Tuplet_spanner::get_default_dir () const
{
  Direction d = UP;
  SCM dir_sym =get_elt_property ("dir-forced");
  if (dir_sym != SCM_UNDEFINED) {
    d= (Direction) gh_scm2int (dir_sym);
    if (d != CENTER)
      return d;
  }

  for (SCM s = get_elt_property ("columns"); gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element * sc = unsmob_element (gh_car (s));
      Note_column * nc = dynamic_cast<Note_column*> (sc);
      if (nc->dir () < 0) 
	{
	  d = DOWN;
	  break;
	}
    }
  
  return d;
}

void
Tuplet_spanner::add_beam (Beam *b)
{
  add_dependency (b);
  Group_interface gi (this, "beams");
  gi.add_element (b);
}

void
Tuplet_spanner::add_column (Note_column*n)
{
  Group_interface gi (this, "columns");
  gi.add_element (n);

  add_dependency (n);
}


