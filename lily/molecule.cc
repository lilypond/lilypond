/*
  molecule.cc -- implement Molecule

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "interval.hh"
#include "string.hh"
#include "molecule.hh"
#include "atom.hh"
#include "debug.hh"

#include "killing-cons.tcc"

Box
Molecule::extent() const
{
  return dim_;
}

Interval
Molecule::extent(Axis a) const
{
  return dim_[a];
}

void
Molecule::translate (Offset o)
{
  for (Cons<Atom> *  ptr = atom_list_; ptr; ptr = ptr->next_)
    {
      ptr->car_->off_ += o;
    }
  dim_.translate (o);
}

void
Molecule::translate_axis (Real x,Axis a)
{
  for (Cons<Atom> *  ptr = atom_list_; ptr; ptr = ptr->next_)
      ptr->car_->off_[a] += x;

  dim_[a] += x;
}

void
Molecule::add_molecule (Molecule const &m)
{
  Cons_list<Atom> al;
  copy_killing_cons_list (al, m.atom_list_);

  if (al.head_)
    {
      *al.tail_ = atom_list_;
      atom_list_ = al.head_;
      al.head_ =0;
    }
  dim_.unite (m.dim_);
}

void
Molecule::add_atom (Atom const *al)
{
  Atom *a = new Atom(*al);

  atom_list_ = new Killing_cons<Atom> (a, atom_list_);
}

void
Molecule::operator=(Molecule const & src)
{
  if (&src == this) return;
  delete atom_list_;
  atom_list_ = 0;
  dim_= src.dim_;
  add_molecule (src);
}

Molecule::Molecule (Molecule const &s)
{
  atom_list_ = 0;
  add_molecule (s);
}

Molecule::~Molecule ()
{
  delete atom_list_;
}

void
Molecule::print() const
{
#ifndef NPRINT
  if (! check_debug)
    return;
  DOUT << "dim:";
  for (Axis i=X_AXIS; i < NO_AXES; incr (i))
    DOUT << axis_name_str (i) << " = " << dim_[i].str ();
#endif
}

void
Molecule::do_center (Axis a)
{
  Interval i (extent (a));
  translate_axis (-i.center (), a);
}

Molecule::Molecule ()
{
  dim_ = Box (Interval(0,0),Interval( 0,0  ));
  atom_list_ = 0;
}


void
Molecule::add_at_edge (Axis a, Direction d, Molecule const &m, Real padding)
{
  Real my_extent= dim_[a][d];
  
  Real offset = my_extent -  m.extent ()[a][-d];
  Molecule toadd (m);
  toadd.translate_axis (offset + d * padding, a);
  add_molecule (toadd);
}
