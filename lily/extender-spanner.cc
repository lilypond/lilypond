/*
  extender-spanner.cc -- implement Extender_spanner

  source file of the GNU LilyPond music typesetter

  (c) 1998, 1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#include "box.hh"
#include "debug.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "p-col.hh"
#include "paper-def.hh"
#include "extender-spanner.hh"

Extender_spanner::Extender_spanner ()
  : Directional_spanner ()
{
  item_l_drul_[LEFT] = item_l_drul_[RIGHT] = 0;
  dy_f_drul_[LEFT] = dy_f_drul_[RIGHT] = 0.0;
  dx_f_drul_[LEFT] = dx_f_drul_[RIGHT] = 0.0;
}

Extender_spanner::Extender_spanner (Extender_spanner const& c)
  : Directional_spanner (c)
{
  item_l_drul_ = c.item_l_drul_;
  dy_f_drul_ = c.dy_f_drul_;
  dx_f_drul_ = c.dx_f_drul_;
}

Extender_spanner::~Extender_spanner ()
{
}

Offset
Extender_spanner::center () const
{
  Real dx = extent (X_AXIS).length ();

  return Offset (dx / 2, 0);
}

Molecule*
Extender_spanner::do_brew_molecule_p () const
{
  Molecule* mol_p = new Molecule;

  Real w = extent (X_AXIS).length ();
  
  w += (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT]);
  Real h = paper ()->get_realvar (ly_symbol ("extender_height"));
  Molecule a = lookup_l ()->filledbox ( Box (Interval (0,w), Interval (0,h)));
  a.translate (Offset (dx_f_drul_[LEFT], dy_f_drul_[LEFT]));

  mol_p->add_molecule (a);

  return mol_p;
}

void
Extender_spanner::do_add_processing ()
{
  Direction d = LEFT;
  Drul_array<Item *> new_textitem_drul = item_l_drul_;
  do {
    if (!item_l_drul_[d])
      new_textitem_drul[d] = item_l_drul_[(Direction)-d];
  } while (flip(&d) != LEFT);
  item_l_drul_ = new_textitem_drul;
}

Interval
Extender_spanner::do_height () const
{
  return Interval (0,0);
}

void
Extender_spanner::do_post_processing ()
{
  assert (item_l_drul_[LEFT] || item_l_drul_[RIGHT]);

  // UGH
  Real nw_f = paper ()->note_width () * 0.8;

  Direction d = LEFT;
  do
    {
      Item* t = item_l_drul_[d]
	? item_l_drul_[d] : item_l_drul_[(Direction)-d];
      if (d == LEFT)
        dx_f_drul_[d] += t->extent (X_AXIS).length ();
      else
	dx_f_drul_[d] -= d * nw_f / 2;
    }
  while (flip(&d) != LEFT);
}

void
Extender_spanner::do_substitute_element_pointer (Score_element* o, Score_element* n)
{
  Item* new_l = n ? dynamic_cast<Item *> (n) : 0;
  if (dynamic_cast <Item *> (o) == item_l_drul_[LEFT])
    item_l_drul_[LEFT] = new_l;
  else if (dynamic_cast <Item *> (o) == item_l_drul_[RIGHT])
    item_l_drul_[RIGHT] = new_l;
}
  
void
Extender_spanner::set_textitem (Direction d, Item* textitem_l)
{
  assert (!item_l_drul_[d]);
  item_l_drul_[d] = textitem_l;
  set_bounds (d, textitem_l);

  add_dependency (textitem_l);
}

