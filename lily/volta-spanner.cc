/*
  volta-spanner.cc -- implement Volta_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "atom.hh"
#include "box.hh"
#include "debug.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "note-column.hh"
#include "p-col.hh"
#include "paper-def.hh"
#include "volta-spanner.hh"
#include "stem.hh"
#include "text-def.hh"

/*
   Hmm, should probably make generic Bracket_spanner,
   or and derive Plet and volta spanner from that.
 */

Volta_spanner::Volta_spanner ()
{
  dir_ = UP;
  last_b_ = false;
  tdef_p_.set_p (new Text_def);
  tdef_p_->align_dir_ = CENTER;
  tdef_p_->style_str_ = "nummer";
}

Molecule*
Volta_spanner::brew_molecule_p () const
{
  Molecule* mol_p = new Molecule;

  if (column_arr_.size ()){
    Real w = width ().length ();
    Real dy = column_arr_.top ()->extent (Y_AXIS) [dir_]
      - column_arr_[0]->extent (Y_AXIS) [dir_];

    Atom num (tdef_p_->get_atom (paper (), CENTER));
    mol_p->add_atom (num);
    mol_p->add_atom (lookup_l ()->volta (w, last_b_));
  }
  return mol_p;
}
  
void
Volta_spanner::do_add_processing ()
{
  if (column_arr_.size ())
    {
      set_bounds (LEFT, column_arr_[0]);
      set_bounds (RIGHT, column_arr_.top ());  
    }
}
  
void
Volta_spanner::do_post_processing ()
{
    if (column_arr_.size())
    	translate_axis (column_arr_[0]->extent (Y_AXIS)[dir_], Y_AXIS);
}

void
Volta_spanner::do_substitute_dependency (Score_element* o, Score_element* n)
{
  if (Note_column *onc = dynamic_cast <Note_column *> (o))
    column_arr_.substitute (onc, dynamic_cast<Note_column*> (n));
}
  
void
Volta_spanner::add_column (Note_column*n)
{
  column_arr_.push (n);
  add_dependency (n);
}

