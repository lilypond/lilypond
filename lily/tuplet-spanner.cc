/*
  plet-spanner.cc -- implement Plet_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/
/*
  todo: handle breaking elegantly.
 */
#include "beam.hh"
#include "atom.hh"
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

Plet_spanner::Plet_spanner ()
{
  beam_l_ =0;
  bracket_visibility_b_ = true;
  num_visibility_b_ = true;
  
  tdef_p_.set_p(new Text_def);
  tdef_p_->align_dir_ = CENTER;
  tdef_p_->style_str_ = "italic";
}

Molecule*
Plet_spanner::do_brew_molecule_p () const
{
  Molecule* mol_p = new Molecule;

  if (column_arr_.size ()){
    Real ncw = column_arr_.top ()->extent (X_AXIS).length ();
    Atom num (tdef_p_->get_atom (paper (), CENTER));

    if (beam_l_ && !bracket_visibility_b_)
      {
	num.translate (((Directional_spanner*)beam_l_)->center ());
	num.translate_axis (ncw, X_AXIS);
      }
    
    if (bracket_visibility_b_)      
      {
	Real dy = column_arr_.top ()->extent (Y_AXIS) [dir_]
	  - column_arr_[0]->extent (Y_AXIS) [dir_];
	Real w = extent (X_AXIS).length () + ncw;

	num.translate (Offset (w/2, dy/2));
	mol_p->add_atom (lookup_l ()->plet (dy, w, dir_));
      }

    if (num_visibility_b_)
      mol_p->add_atom (num);
  }
  return mol_p;
}
  
void
Plet_spanner::do_add_processing ()
{
  if (column_arr_.size ())
    {
      set_bounds (LEFT, column_arr_[0]);
      set_bounds (RIGHT, column_arr_.top ());  
    }
}
  
void
Plet_spanner::do_post_processing ()
{
  if (column_arr_.size())
    translate_axis (column_arr_[0]->extent (Y_AXIS)[dir_], Y_AXIS);

  if (!broken_into_l_arr_.size () && beam_l_
      && spanned_drul_[LEFT]->column_l () == beam_l_->spanned_drul_[LEFT]->column_l ()
      && spanned_drul_[RIGHT]->column_l () == beam_l_->spanned_drul_[RIGHT]->column_l ())
    bracket_visibility_b_ = false;

  if (column_arr_.size () == 1)
    bracket_visibility_b_ = false;
}

void
Plet_spanner::do_substitute_dependency (Score_element* o, Score_element* n)
{
  if (Note_column *onc = dynamic_cast <Note_column *> (o))
    column_arr_.substitute (onc, dynamic_cast<Note_column*> (n));
  else if (Beam *b = dynamic_cast<Beam *> (o))
    {
      if (b == beam_l_) 
	beam_l_ = dynamic_cast<Beam*> (n);
    }
}
  
void
Plet_spanner::set_default_dir ()
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
Plet_spanner::set_beam (Beam *b)
{
  beam_l_ = b;
  add_dependency (b);
}

void
Plet_spanner::add_column (Note_column*n)
{
  column_arr_.push (n);
  add_dependency (n);
}

