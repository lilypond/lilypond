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
#define NEXT_CELL(a) SCM_CDR(a)
#define CELLTYPE SCM
#define UNBOX_ATOM(a) Atom::atom_l (a)
#define BOX_ATOM(a) a->make_smob ()
#define NEWCELL(a,b) gh_cons (a,b)
#define UNBOX_PTR(a) SCM_CAR(a)
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
  dim_.translate (o);
}

void
Molecule::translate_axis (Real x,Axis a)
{
  for (CELLTYPE  ptr = atom_list_; ptr != MOL_EOL; ptr = NEXT_CELL(ptr))
    UNBOX_ATOM (UNBOX_PTR(ptr))->off_[a] += x;

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
  if (&src == this)
  return;



#ifndef ATOM_SMOB
  delete atom_list_;
#endif

  atom_list_ = MOL_EOL;
  dim_= src.dim_;
  add_molecule (src);
}

Molecule::Molecule (Molecule const &s)
{
  atom_list_ = MOL_EOL;
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
  atom_list_ = MOL_EOL;
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
