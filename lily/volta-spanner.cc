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
#include "dimension-cache.hh"
#include "group-interface.hh"

Volta_spanner::Volta_spanner ()
{
  last_b_ = false;
  dim_cache_ [Y_AXIS]->set_callback (dim_callback);
  set_elt_property ("bars", SCM_EOL);
  set_elt_property ("note-columns", SCM_EOL);
}

Molecule*
Volta_spanner::do_brew_molecule_p () const
{
  Molecule* mol_p = new Molecule;

  Link_array<Bar> bar_arr
    = Group_interface__extract_elements (this, (Bar*)0, "bars");

  if (!bar_arr.size ())
    return mol_p;

  Link_array<Bar> note_column_arr
    = Group_interface__extract_elements (this, (Bar*)0, "note-columns");

  
  bool no_vertical_start = false;
  bool no_vertical_end = last_b_;
  Spanner *orig_span =  dynamic_cast<Spanner*> (original_l_);
  if (orig_span && (orig_span->broken_into_l_arr_[0] != (Spanner*)this))
    no_vertical_start = true;
  if (orig_span && (orig_span->broken_into_l_arr_.top () != (Spanner*)this))
    no_vertical_end = true;
  if (bar_arr.top ()->type_str_.length_i () > 1)
    no_vertical_end = false;

  Real interline_f = paper_l ()->get_var ("interline");
  Real internote_f = interline_f/2;
  Real t = paper_l ()->get_var ("volta_thick");

  Real dx = internote_f;
  Real w = spanner_length() - dx - get_broken_left_end_align ();
  Real h = paper_l()->get_var ("volta_spanner_height");
  Molecule volta (lookup_l ()->volta (h, w, t, no_vertical_start, no_vertical_end));

  
  Molecule num (lookup_l ()->text ("volta", number_str_, paper_l ()));
  Real dy = bar_arr.top ()->extent (Y_AXIS) [UP] > 
     bar_arr[0]->extent (Y_AXIS) [UP];
  dy += 2 * h;

  for (int i = 0; i < note_column_arr.size (); i++)
    dy = dy >? note_column_arr[i]->extent (Y_AXIS)[BIGGER];
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

  Link_array<Bar> bar_arr
    = Group_interface__extract_elements (this, (Bar*)0, "bars");

  if (bar_arr.size ())
    {
      set_bounds (LEFT, bar_arr[0]);
      set_bounds (RIGHT, bar_arr.top ());  
    }
}

/*
    Originally the following comment existed here
    "in most cases, it's a lot better not no have height...",
    but problems existed with collision between volta spanner
    and above staff or lyrics for multi-staff music, so the proper
    height is now being returned. Additional space should still
    be added elsewhere so lyrics from above staff do not sit on
    volta spanner. (Roy R. Rankin)
*/
Interval
Volta_spanner::dim_callback (Dimension_cache const *c)
{
  Volta_spanner * v = dynamic_cast<Volta_spanner*> (c->element_l ());
  Real h = v->paper_l()->get_var ("volta_spanner_height") * 2.;
  return Interval (0., h);
}

void
Volta_spanner::do_post_processing ()
{

  Link_array<Bar> bar_arr
    = Group_interface__extract_elements (this, (Bar*)0, "bars");
  
  if (bar_arr.size())
    translate_axis (bar_arr[0]->extent (Y_AXIS)[UP], Y_AXIS);
  translate_axis (get_broken_left_end_align (), X_AXIS);
}


  
void
Volta_spanner::add_bar  (Bar* c)
{
  Group_interface gi(this, "bars");
  gi.add_element (c);

  add_dependency (c);
}

void
Volta_spanner::add_column (Note_column* c)
{
  Group_interface gi(this, "note-columns");
  gi.add_element (c);

  add_dependency (c);
}


