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
#include "p-col.hh"
#include "paper-def.hh"
#include "tuplet-spanner.hh"
#include "stem.hh"
#include "note-column.hh"

Tuplet_spanner::Tuplet_spanner ()
{
  bracket_visibility_b_ = true;
  num_visibility_b_ = true;
}

/*
  TODO.  We should control the gap for lookup from here.
 */
Molecule*
Tuplet_spanner::do_brew_molecule_p () const
{
  Molecule* mol_p = new Molecule;

  if (column_arr_.size ()){
    Real ncw = column_arr_.top ()->extent (X_AXIS).length ();
    Molecule num (lookup_l ()->text ("italic",
				     number_str_));
    num.align_to (X_AXIS, CENTER);
    num.translate_axis (dir_ * paper_l ()->get_realvar (interline_scm_sym),  Y_AXIS);

    if (beam_l_arr_.size () == 1 && !bracket_visibility_b_)
      {
	Beam *beam_l = beam_l_arr_[0];
	Directional_spanner* ds = dynamic_cast<Directional_spanner*>(beam_l);
	num.translate (ds->center ());
	num.translate_axis (ncw, X_AXIS);
      }
    
    if (bracket_visibility_b_)      
      {
	Real dy = column_arr_.top ()->extent (Y_AXIS) [dir_]
	  - column_arr_[0]->extent (Y_AXIS) [dir_];
	Real w = extent (X_AXIS).length () + ncw;
	//	num.align_to (Y_AXIS, CENTER);
	num.translate (Offset (w/2, dy/2));
	mol_p->add_molecule (lookup_l ()->plet (dy, w, dir_));
      }

    if (num_visibility_b_)
      {
	mol_p->add_molecule (num);
      }
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
    translate_axis (column_arr_[0]->extent (Y_AXIS)[dir_], Y_AXIS);

  if (beam_l_arr_.size () == 1)
    {
      Beam * beam_l = beam_l_arr_[0];
      if (!broken_b () 
	  && spanned_drul_[LEFT]->column_l () == beam_l->spanned_drul_[LEFT]->column_l ()
	  && spanned_drul_[RIGHT]->column_l () == beam_l->spanned_drul_[RIGHT]->column_l ())
	bracket_visibility_b_ = false;
    }

  if (column_arr_.size () == 1)
    bracket_visibility_b_ = false;
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

