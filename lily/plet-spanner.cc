/*
  plet-spanner.cc -- implement Plet_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "atom.hh"
#include "box.hh"
#include "debug.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "p-col.hh"
#include "paper-def.hh"
#include "plet-spanner.hh"
#include "stem.hh"
#include "text-def.hh"


/*
  UHGUGH THIS IS BROKEN! do not derive from Bow
 */
IMPLEMENT_IS_TYPE_B1 (Plet_spanner,Bow);
  
Plet_spanner::Plet_spanner ()
  : Bow ()
{
  stem_l_drul_[RIGHT] =0;
  stem_l_drul_[LEFT] =0;
  visibility_i_ = 3;

  tdef_p_ = new Text_def;
  tdef_p_->align_dir_ = CENTER;
  tdef_p_->style_str_ = "italic";
}

Plet_spanner::Plet_spanner (Plet_spanner const& c)
  : Bow (c)
{
  tdef_p_ = new Text_def (*c.tdef_p_);
  stem_l_drul_ = c.stem_l_drul_;
  visibility_i_ = c.visibility_i_;
}

Plet_spanner::~Plet_spanner ()
{
  delete tdef_p_;
}

Molecule*
Plet_spanner::brew_molecule_p () const
{
  Molecule* mol_p = new Molecule;

  Real w = width ().length ();
  
  Real dy_f = dy_f_drul_[RIGHT] - dy_f_drul_[LEFT];
  
  w += (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT]);
  
  Atom a = lookup_l ()->plet (dy_f, w, dir_);

  a.translate (Offset (dx_f_drul_[LEFT], dy_f_drul_[LEFT]));

  if (visibility_i_ >= 2)
      mol_p->add_atom (a);

  Real interline_f = paper ()->interline_f ();
  Real numy_f = (dir_ > 0 ? 0 : -interline_f) + dir_ * interline_f / 2;
  Atom num (tdef_p_->get_atom (paper (), CENTER));
  num.translate (Offset (width ().length () / 1.8 + dx_f_drul_[LEFT], 
    dy_f_drul_[LEFT] + dy_f / 2 + numy_f));

  if (visibility_i_ >= 1)
    mol_p->add_atom (num);

  return mol_p;
}
  
void
Plet_spanner::do_add_processing ()
{
  if (! (stem_l_drul_[LEFT] && stem_l_drul_[RIGHT]))
    warning (_ ("lonely plet"));

  Direction d = LEFT;
  Drul_array<Stem *> new_stem_drul = stem_l_drul_;
  do {
    if (!stem_l_drul_[d])
      new_stem_drul[d] = stem_l_drul_[(Direction)-d];
  } while (flip(&d) != LEFT);
  stem_l_drul_ = new_stem_drul;
}
  
void
Plet_spanner::do_post_processing ()
{
  Real interline_f = paper ()->interline_f ();
  Real nh_f = interline_f / 2;
  assert (stem_l_drul_[LEFT] || stem_l_drul_[RIGHT]);

  // ugh
  Real nw_f = paper ()->note_width () * 0.8;

  Direction d = LEFT;
  do
    {
      Stem* s = stem_l_drul_[d] ? stem_l_drul_[d] : stem_l_drul_[(Direction)-d];

      dy_f_drul_[d] = dir_ == s->get_dir () ? s->stem_end_f ()
	  : s->stem_begin_f () + dir_ * nh_f / 2;
      dy_f_drul_[d] *= .5 * interline_f;
      dy_f_drul_[d] += dir_ * interline_f;
      if (d == RIGHT)
        dx_f_drul_[d] = nw_f;
    }
  while (flip(&d) != LEFT);
  do {
    if (stem_l_drul_[d]->empty_b ())
      {
        Direction u = d;
	flip (&u);
	dy_f_drul_[d] = dy_f_drul_[u]; // ughugh \[/3 r8 c8 r8 \]/1
      }
    }
  while (flip(&d) != LEFT);
}

void
Plet_spanner::do_substitute_dependency (Score_element* o, Score_element* n)
{
  Stem* new_l = n ? (Stem*)n->access_Item () : 0;
  if (o->access_Item () == stem_l_drul_[LEFT])
    stem_l_drul_[LEFT] = new_l;
  else if (o->access_Item () == stem_l_drul_[RIGHT])
    stem_l_drul_[RIGHT] = new_l;
}
  
void
Plet_spanner::set_default_dir ()
{
  Real m = (stem_l_drul_[LEFT]->stem_end_f () 
	    + stem_l_drul_[RIGHT]->stem_end_f ()) / 2;
  dir_ =  (m < 0) ? DOWN : UP;
}

void
Plet_spanner::set_stem (Direction d, Stem* stem_l)
{
  assert (!stem_l_drul_[d]);
  stem_l_drul_[d] = stem_l;
  set_bounds (d, stem_l);

  add_dependency (stem_l);
}

