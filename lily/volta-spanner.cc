/*
  volta-spanner.cc -- implement Volta_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#include "box.hh"
#include "debug.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "note-column.hh"
#include "paper-column.hh"
#include "bar.hh"
#include "paper-def.hh"
#include "volta-spanner.hh"
#include "stem.hh"

#include "pointer.tcc"

Volta_spanner::Volta_spanner ()
{
  last_b_ = false;
}

Molecule*
Volta_spanner::do_brew_molecule_p () const
{
  Molecule* mol_p = new Molecule;

  if (!bar_arr_.size ())
    return mol_p;
  
  bool no_vertical_start = false;
  bool no_vertical_end = last_b_;
  Spanner *orig_span =  dynamic_cast<Spanner*> (original_l_);
  if (orig_span && (orig_span->broken_into_l_arr_[0] != (Spanner*)this))
    no_vertical_start = true;
  if (orig_span && (orig_span->broken_into_l_arr_.top () != (Spanner*)this))
    no_vertical_end = true;
  if (bar_arr_.top ()->type_str_.length_i () > 1)
    no_vertical_end = false;

  Real interline_f = paper_l ()->get_realvar (interline_scm_sym);
  Real internote_f = interline_f/2;
  Real t = paper_l ()->get_realvar (volta_thick_scm_sym);

  Real dx = internote_f;
  Real w = extent (X_AXIS).length () - dx - get_broken_left_end_align ();
  Real h = paper_l()->get_var ("volta_spanner_height");
  Molecule volta (lookup_l ()->volta (h, w, t, no_vertical_start, no_vertical_end));

  
  Molecule num (lookup_l ()->text ("volta", number_str_, paper_l ()));
  Real dy = bar_arr_.top ()->extent (Y_AXIS) [UP] > 
     bar_arr_[0]->extent (Y_AXIS) [UP];
  dy += 2 * h;

  for (int i = 0; i < note_column_arr_.size (); i++)
    dy = dy >? note_column_arr_[i]->extent (Y_AXIS)[BIGGER];
  dy -= h;

  Molecule two (lookup_l ()->text ("number", "2", paper_l ()));
  Real gap = two.dim_.x ().length () / 2;
  Offset off (num.dim_.x ().length () + gap, 
	      h / internote_f - gap);
  num.translate (off);
  mol_p->add_molecule (volta);
  mol_p->add_molecule (num);
  mol_p->translate (Offset (0, dy));
  return mol_p;
}
  
void
Volta_spanner::do_add_processing ()
{
  if (bar_arr_.size ())
    {
      set_bounds (LEFT, bar_arr_[0]);
      set_bounds (RIGHT, bar_arr_.top ());  
    }
}
  
Interval
Volta_spanner::do_height () const
{
  /*
    in most cases, it's a lot better not no have height...
  */
  Interval i;
  return i;
}

void
Volta_spanner::do_post_processing ()
{
  if (bar_arr_.size())
    translate_axis (bar_arr_[0]->extent (Y_AXIS)[UP], Y_AXIS);
  translate_axis (get_broken_left_end_align (), X_AXIS);
}

void
Volta_spanner::do_substitute_element_pointer (Score_element* o, Score_element* n)
{
  if (Note_column* c = dynamic_cast <Note_column*> (o))
    note_column_arr_.substitute (c, dynamic_cast<Note_column*> (n));
  else if (Bar* c = dynamic_cast <Bar*> (o))
    bar_arr_.substitute (c, dynamic_cast<Bar*> (n));
}
  
void
Volta_spanner::add_bar  (Bar* c)
{
  bar_arr_.push (c);
  add_dependency (c);
}

void
Volta_spanner::add_column (Note_column* c)
{
  note_column_arr_.push (c);
  add_dependency (c);
}

