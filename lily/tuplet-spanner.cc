/*
  plet-spanner.cc -- implement Tuplet_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/
/*
  todo: handle breaking elegantly.
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
#include "text-def.hh"
#include "note-column.hh"

Tuplet_spanner::Tuplet_spanner ()
{
  beam_l_ =0;
  bracket_visibility_b_ = true;
  num_visibility_b_ = true;
  
  tdef_p_.set_p(new Text_def);
  tdef_p_->align_dir_ = CENTER;
  tdef_p_->style_str_ = "italic";
}

Molecule*
Tuplet_spanner::do_brew_molecule_p () const
{
  Molecule* mol_p = new Molecule;

  if (column_arr_.size ()){
    Real ncw = column_arr_.top ()->extent (X_AXIS).length ();
    Molecule num (tdef_p_->get_molecule (paper_l (), CENTER));

    if (beam_l_ && !bracket_visibility_b_)
      {
	Directional_spanner* ds = dynamic_cast<Directional_spanner*>(beam_l_);
	num.translate (ds->center ());
	num.translate_axis (ncw, X_AXIS);
      }
    
    if (bracket_visibility_b_)      
      {
	Real dy = column_arr_.top ()->extent (Y_AXIS) [dir_]
	  - column_arr_[0]->extent (Y_AXIS) [dir_];
	Real w = extent (X_AXIS).length () + ncw;

	num.translate (Offset (w/2, dy/2));
	mol_p->add_molecule (lookup_l ()->plet (dy, w, dir_));
      }

    if (num_visibility_b_)
      mol_p->add_molecule (num);
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

  if (!broken_b () && beam_l_
      && spanned_drul_[LEFT]->column_l () == beam_l_->spanned_drul_[LEFT]->column_l ()
      && spanned_drul_[RIGHT]->column_l () == beam_l_->spanned_drul_[RIGHT]->column_l ())
    bracket_visibility_b_ = false;

  if (column_arr_.size () == 1)
    bracket_visibility_b_ = false;
}

void
Tuplet_spanner::do_substitute_element_pointer (Score_element* o, Score_element* n)
{
  if (Note_column *onc = dynamic_cast <Note_column *> (o))
    column_arr_.substitute (onc, dynamic_cast<Note_column*> (n));
  else if (o == beam_l_)
    {
      beam_l_ = dynamic_cast<Beam*> (n);
    }
}

void
Tuplet_spanner::set_default_dir ()
{
  dir_ = UP;
  for (int i=0; i < column_arr_.size (); i ++) 
    {
      if (column_arr_[i]->dir_ < 0) 
	{
	  dir_ = DOWN;
	  break;
	}
    }
}

void
Tuplet_spanner::set_beam (Beam *b)
{
  assert(!beam_l_);
  beam_l_ = b;
  add_dependency (b);
}

void
Tuplet_spanner::add_column (Note_column*n)
{
  column_arr_.push (n);
  add_dependency (n);
}

