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
#include "p-col.hh" // urg
#include "bar.hh"
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
  number_p_.set_p (new Text_def);
  number_p_->align_dir_ = LEFT;
  dot_p_.set_p (new Text_def);
  dot_p_->align_dir_ = LEFT;
}

Molecule*
Volta_spanner::brew_molecule_p () const
{
  Molecule* mol_p = new Molecule;

  if (column_arr_.size ())
    {
      Real internote_f = paper ()->internote_f ();
      Real dx = internote_f;
      Real w = width ().length () - 2 * dx;
      Atom volta (lookup_l ()->volta (w, last_b_));
      Real h = volta.dim_.y ().length ();
      Atom num (number_p_->get_atom (paper (), LEFT));
      Atom dot (dot_p_->get_atom (paper (), LEFT));
      Real dy = column_arr_.top ()->extent (Y_AXIS) [dir_] > 
	column_arr_[0]->extent (Y_AXIS) [dir_];
      dy += 2 * h;
      for (int i = 0; i < note_column_arr_.size (); i++)
        dy = dy >? note_column_arr_[i]->height ().max ();
      dy -= h;

      Real gap = num.dim_.x ().length () / 2;
      Offset off (num.dim_.x ().length () + gap, 
        (h - num.dim_.y ().length ()) / internote_f - gap);
      num.translate (off);
      Real dotheight = dot.dim_.y ().length () / 7;
      off -= Offset (0, dotheight);
      dot.translate (off);
      mol_p->add_atom (volta);
      mol_p->add_atom (num);
      mol_p->add_atom (dot);
      mol_p->translate (Offset (dx, dy));
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
  number_p_->style_str_ = "number-1";
  dot_p_->text_str_ = ".";
  dot_p_->style_str_ = "bold";
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
  if (Note_column* c = dynamic_cast <Note_column*> (o))
    note_column_arr_.substitute (c, dynamic_cast<Note_column*> (n));
  else if (Bar* c = dynamic_cast <Bar*> (o))
    column_arr_.substitute (c, dynamic_cast<Bar*> (n));
}
  
void
Volta_spanner::add_column (Bar* c)
{
  column_arr_.push (c);
  add_dependency (c);
}

void
Volta_spanner::add_column (Note_column* c)
{
  note_column_arr_.push (c);
  add_dependency (c);
}

