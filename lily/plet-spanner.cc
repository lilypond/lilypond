/*
  plet-spanner.cc -- implement Plet_spanner

  source file of the GNU LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "atom.hh"
#include "boxes.hh"
#include "debug.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "p-col.hh"
#include "paper-def.hh"
#include "plet-spanner.hh"
#include "stem.hh"
#include "text-def.hh"

IMPLEMENT_IS_TYPE_B1 (Plet_spanner,Bow);
  
Plet_spanner::Plet_spanner ()
  : Bow ()
{
  stem_l_drul_[RIGHT] =0;
  stem_l_drul_[LEFT] =0;

  tdef_p_ = new Text_def;
  tdef_p_->align_i_ = CENTER;
  tdef_p_->style_str_ = "italic";
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
  
  // ugh
  Real nwc_f = (dir_ > 0 ? paper ()->note_width () : 0) * 0.8;
  
  w += (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT]);
  
  Atom a = paper ()->lookup_l ()->plet (dy_f, w, dir_);

  a.translate (Offset ( (dx_f_drul_[LEFT] + nwc_f), dy_f_drul_[LEFT]));
  mol_p->add (a);

  Real interline_f = paper ()->interline_f ();
  Real numy_f = (dir_ > 0 ? 0 : -interline_f / 2);
  Real numx_f = interline_f / 1.5;
  Atom num (tdef_p_->get_atom (paper (), CENTER));
  num.translate (Offset (width ().length ()/ 2 + nwc_f - numx_f 
			 + dx_f_drul_[LEFT], 
			 dy_f_drul_[LEFT] + dy_f / width ().length () / 2 
			 + dir_ * interline_f / 2 + numy_f));
  mol_p->add (num);

  return mol_p;
}
  
void
Plet_spanner::do_add_processing ()
{
  if (! (stem_l_drul_[LEFT] && stem_l_drul_[RIGHT]))
    warning (_ ("Lonely plet.. "));

  Direction d = LEFT;
  Drul_array<Stem *> new_stem_drul = stem_l_drul_;
  do {
    if (!stem_l_drul_[d])
      new_stem_drul[d] = stem_l_drul_[(Direction)-d];
  } while ( (d *= -1) != LEFT);
  stem_l_drul_ = new_stem_drul;
}
  
void
Plet_spanner::do_post_processing ()
{
  Real interline_f = paper ()->interline_f ();
  assert (stem_l_drul_[LEFT] || stem_l_drul_[RIGHT]);

  Direction d = LEFT;
  do
    {
      dy_f_drul_[d] = .5 * interline_f * (stem_l_drul_[d] 
					  ? stem_l_drul_[d]->stem_end_f ()
					  : stem_l_drul_[(Direction)-d]->stem_end_f ());
      dy_f_drul_[d] += dir_ * interline_f;
    }
  while ( (d *= -1) != LEFT);
}

void
Plet_spanner::do_substitute_dependency (Score_elem* o, Score_elem* n)
{
  Stem* new_l = n ? (Stem*)n->item () : 0;
  if (o->item () == stem_l_drul_[LEFT])
    stem_l_drul_[LEFT] = new_l;
  else if (o->item () == stem_l_drul_[RIGHT])
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

