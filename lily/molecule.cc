/*
  molecule.cc -- implement Molecule

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>

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
  if (isinf (o.length ()))
    {
      programming_error ("Translating infinitely. Ignore.");
      return;
    }
    
  for (SCM ptr = gh_cdr (atom_list_);  ptr != SCM_EOL; ptr = gh_cdr(ptr))
    {
      unsmob_atom (gh_car (ptr))->off_ += o;
    }
  if (!empty_b ())
    dim_.translate (o);
}

void
Molecule::translate_axis (Real x,Axis a)
{
  if (isinf (x))
    {
      programming_error ("Translating infinitely. Ignore.");
      return;
    }
  for (SCM ptr = gh_cdr (atom_list_);  ptr != SCM_EOL; ptr = gh_cdr(ptr))
    {
      unsmob_atom (gh_car (ptr))->off_[a] += x;
    }

  if (!dim_[a].empty_b ())
    dim_[a] += x;
}

void
Molecule::add_molecule (Molecule const &m)
{
  for (SCM ptr = gh_cdr (m.atom_list_);  ptr != SCM_EOL; ptr = gh_cdr(ptr))
    {
      Atom *a = new Atom (*unsmob_atom (gh_car (ptr)));
      add_atom (a->self_scm_);
    }
  dim_.unite (m.dim_);
}

void
Molecule::add_atom (SCM atomsmob)
{
  gh_set_cdr_x (atom_list_,
		gh_cons  (atomsmob, gh_cdr (atom_list_)));

  scm_unprotect_object (atomsmob);
}

void
Molecule::operator=(Molecule const & src)
{
  if (&src == this)
    return;

  atom_list_ = gh_cons (SCM_EOL,SCM_EOL);
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

void
Molecule::print () const
{
#ifndef NPRINT
  for (SCM ptr = gh_cdr (atom_list_);  ptr != SCM_EOL; ptr = gh_cdr(ptr))
    gh_display (gh_car (ptr));
#endif
}

Molecule::Molecule (Molecule const &s)
{
  atom_list_ = gh_cons (SCM_EOL, SCM_EOL);
  set_empty (true);
  add_molecule (s);
}

Molecule::~Molecule ()
{
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
  atom_list_ = gh_cons (SCM_EOL, SCM_EOL);
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
  return gh_cdr (atom_list_) == SCM_EOL;
}
