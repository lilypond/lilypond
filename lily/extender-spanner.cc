/*
  extender-spanner.cc -- implement Extender_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "atom.hh"
#include "box.hh"
#include "debug.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "p-col.hh"
#include "paper-def.hh"
#include "extender-spanner.hh"
#include "text-item.hh"
#include "text-def.hh"

Extender_spanner::Extender_spanner ()
  : Directional_spanner ()
{
  textitem_l_drul_[LEFT] = textitem_l_drul_[RIGHT] = 0;
  dy_f_drul_[LEFT] = dy_f_drul_[RIGHT] = 0.0;
  dx_f_drul_[LEFT] = dx_f_drul_[RIGHT] = 0.0;
}

Extender_spanner::Extender_spanner (Extender_spanner const& c)
  : Directional_spanner (c)
{
  textitem_l_drul_ = c.textitem_l_drul_;
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
  
  Atom a = lookup_l ()->extender (w);

  a.translate (Offset (dx_f_drul_[LEFT], dy_f_drul_[LEFT]));

  mol_p->add_atom (a);

  return mol_p;
}

void
Extender_spanner::do_add_processing ()
{
  Direction d = LEFT;
  Drul_array<Text_item *> new_textitem_drul = textitem_l_drul_;
  do {
    if (!textitem_l_drul_[d])
      new_textitem_drul[d] = textitem_l_drul_[(Direction)-d];
  } while (flip(&d) != LEFT);
  textitem_l_drul_ = new_textitem_drul;
}

Interval
Extender_spanner::do_height () const
{
  return Interval (0,0);
}

void
Extender_spanner::do_post_processing ()
{
  assert (textitem_l_drul_[LEFT] || textitem_l_drul_[RIGHT]);

  // UGH
  Real nw_f = paper ()->note_width () * 0.8;

  Direction d = LEFT;
  do
    {
      Text_item* t = textitem_l_drul_[d] ? textitem_l_drul_[d] : textitem_l_drul_[(Direction)-d];

      dy_f_drul_[d] += t->extent (Y_AXIS).length () / 2;
      if (d == LEFT)
        dx_f_drul_[d] += t->extent (X_AXIS).length ();
      else
	dx_f_drul_[d] -= d * nw_f / 2;

//      dx_f_drul_[d] -= d * nw_f / 4;
    }
  while (flip(&d) != LEFT);
}

void
Extender_spanner::do_substitute_dependency (Score_element* o, Score_element* n)
{
  Text_item* new_l = n ? dynamic_cast<Text_item *> (n) : 0;
  if (dynamic_cast <Item *> (o) == textitem_l_drul_[LEFT])
    textitem_l_drul_[LEFT] = new_l;
  else if (dynamic_cast <Item *> (o) == textitem_l_drul_[RIGHT])
    textitem_l_drul_[RIGHT] = new_l;
}
  
void
Extender_spanner::set_textitem (Direction d, Text_item* textitem_l)
{
  assert (!textitem_l_drul_[d]);
  textitem_l_drul_[d] = textitem_l;
  set_bounds (d, textitem_l);

  add_dependency (textitem_l);
}

