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

#ifdef ATOM_SMOB
#define MOL_EOL SCM_EOL
#define NEXT_CELL(a) gh_cdr(a)
#define CELLTYPE SCM
#define UNBOX_ATOM(a) Atom::atom_l (a)
#define BOX_ATOM(a) a->make_smob ()
#define NEWCELL(a,b) gh_cons (a,b)
#define UNBOX_PTR(a) gh_car (a)
#else
#define MOL_EOL 0
#define NEXT_CELL(a) ptr->next_
#define CELLTYPE Cons<Atom>*
#define UNBOX_ATOM(a) a
#define UNBOX_PTR(a) a->car_
#define BOX_ATOM(a) a
#define NEWCELL(a,b) new Killing_cons<Atom>(a,b)
#endif

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
  for (CELLTYPE ptr = atom_list_; ptr != MOL_EOL; ptr = NEXT_CELL(ptr))
    {
      UNBOX_ATOM(UNBOX_PTR(ptr))->off_ += o;
    }
  if (!empty_b ())
    dim_.translate (o);
}

void
Molecule::translate_axis (Real x,Axis a)
{
  for (CELLTYPE  ptr = atom_list_; ptr != MOL_EOL; ptr = NEXT_CELL(ptr))
    UNBOX_ATOM (UNBOX_PTR(ptr))->off_[a] += x;

  if (!dim_[a].empty_b ())
    dim_[a] += x;
}

void
Molecule::add_molecule (Molecule const &m)
{
  for (CELLTYPE  ptr = m.atom_list_; ptr != MOL_EOL; ptr = NEXT_CELL(ptr))
    add_atom(UNBOX_ATOM (UNBOX_PTR(ptr)));
  
  dim_.unite (m.dim_);
}

void
Molecule::add_atom (Atom const *al)
{
  Atom *a = new Atom(*al);

  atom_list_ = NEWCELL(BOX_ATOM(a), atom_list_);
}

void
Molecule::operator=(Molecule const & src)
{
  if (&src == this) return;

#ifndef ATOM_SMOB
  delete atom_list_;
#endif

  atom_list_ = MOL_EOL;
  dim_= src.dim_;
  add_molecule (src);
}

void
Molecule::set_empty (bool e)
{
  if (e)
    {
      dim_[X_AXIS].set_empty ();
      dim_[Y_AXIS].set_empty ();
    }
  else
    {
      dim_[X_AXIS] = Interval(0,0);
      dim_[Y_AXIS] = Interval (0,0);
    }
}

Molecule::Molecule (Molecule const &s)
{
  atom_list_ = MOL_EOL;
  set_empty (true);
  add_molecule (s);
}

Molecule::~Molecule ()
{
#ifndef ATOM_SMOB
  delete atom_list_;
#endif
}

void
Molecule::print() const
{
#ifndef NPRINT
  if (! flower_dstream)
    return;
  DEBUG_OUT << "dim:";
  for (Axis i=X_AXIS; i < NO_AXES; incr (i))
    DEBUG_OUT << axis_name_str (i) << " = " << dim_[i].str ();
#endif
}

void
Molecule::align_to (Axis a, Direction d)
{
  if (d == CENTER)
    {
      Interval i (extent (a));
      translate_axis (-i.center (), a);
    }
  else
    {
      translate_axis (-extent (a)[d], a);
    }
}

Molecule::Molecule ()
{
  dim_[X_AXIS].set_empty ();
  dim_[Y_AXIS].set_empty ();
  atom_list_ = MOL_EOL;
}


void
Molecule::add_at_edge (Axis a, Direction d, Molecule const &m, Real padding)
{
  Real my_extent= empty_b () ? 0.0 : dim_[a][d];
  Interval i (m.extent ()[a]);
  if (i.empty_b ())
    programming_error ("Molecule::add_at_edge: adding empty molecule.");
  
  Real his_extent = i[-d];
  Real offset = my_extent -  his_extent;
  Molecule toadd (m);
  toadd.translate_axis (offset + d * padding, a);
  add_molecule (toadd);
}

bool
Molecule::empty_b () const
{
  return atom_list_ == MOL_EOL;
}
