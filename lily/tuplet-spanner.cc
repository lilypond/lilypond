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


Tuplet_spanner::Tuplet_spanner ()
{
  parallel_beam_b_ = false;
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
  
  if (column_arr_.size ()){
    Real ncw = column_arr_.top ()->extent (X_AXIS).length ();
    Real w = extent (X_AXIS).length () + ncw;
    Molecule num (lookup_l ()->text ("italic",
				     number_str_, paper_l ()));
    num.align_to (X_AXIS, CENTER);
    num.translate_axis (w/2, X_AXIS);
    Real interline = paper_l ()->get_var ("interline");
    Real dy = column_arr_.top ()->extent (Y_AXIS) [get_direction ()]
      - column_arr_[0]->extent (Y_AXIS) [get_direction ()];
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
  if (column_arr_.size ())
    {
      set_bounds (LEFT, column_arr_[0]);
      set_bounds (RIGHT, column_arr_.top ());  
    }
}
  
void
Tuplet_spanner::do_post_processing ()
{
  if (column_arr_.size())
    translate_axis (column_arr_[0]->extent (Y_AXIS)[get_direction ()], Y_AXIS);

  if (beam_l_arr_.size () == 1)
    {
      Beam * beam_l = beam_l_arr_[0];
      if (!broken_b () 
	  && spanned_drul_[LEFT]->column_l () == beam_l->spanned_drul_[LEFT]->column_l ()
	  && spanned_drul_[RIGHT]->column_l () == beam_l->spanned_drul_[RIGHT]->column_l ())
	parallel_beam_b_ = true;
    }

  //  if (column_arr_.size () == 1)
  //    bracket_visibility_b_ = false;
}

void
Tuplet_spanner::do_substitute_element_pointer (Score_element* o, Score_element* n)
{
  if (Note_column *onc = dynamic_cast <Note_column *> (o))
    column_arr_.substitute (onc, dynamic_cast<Note_column*> (n));
  else if (Beam * b = dynamic_cast<Beam* > (o))
    {
      beam_l_arr_.substitute (b,  dynamic_cast<Beam*> (n));
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

  for (int i=0; i < column_arr_.size (); i ++) 
    {
      if (column_arr_[i]->dir () < 0) 
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
  beam_l_arr_.push (b);
}

void
Tuplet_spanner::add_column (Note_column*n)
{
  column_arr_.push (n);
  add_dependency (n);
}


