/*
  atom.cc -- implement Atom

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "atom.hh"
#include "interval.hh"
#include "string.hh"
#include "array.hh"
#include "debug.hh"
#include "dimensions.hh"
#include "lookup.hh"
#include "main.hh"
#include "global-ctor.hh"

Atom::Atom(SCM s)
{
  func_ = s;
}


#ifdef ATOM_SMOB
int
Atom::smob_display (SCM smob, SCM port, scm_print_state*)
{
  Atom* a =(Atom*) SCM_CDR(smob);
  String i (a->off_.str ());
  
  scm_puts ("#<Atom ", port);
  scm_puts (i.ch_C(), port);
  gh_display (a->func_);
  scm_puts (">", port);

  /* non-zero means success */
  return 1;
}


scm_sizet
Atom::smob_free (SCM smob)
{
  Atom * a= (Atom*) SCM_CDR(smob);
  delete a;
  return sizeof (Atom);
}

SCM
Atom::smob_mark (SCM smob) 
{
  Atom * a= (Atom*) SCM_CDR(smob);
  scm_gc_mark (a->func_);
  return a->font_;
}

long Atom::smob_tag_;

void
Atom::init_smob ()
{
  static scm_smobfuns type_rec;

  type_rec.mark = smob_mark;
  type_rec.free = smob_free;
  type_rec.print = smob_display;
  type_rec.equalp = 0;

  smob_tag_ = scm_newsmob (&type_rec);
}


SCM
Atom::make_smob () const
{
  SCM smob;
  SCM_NEWCELL (smob);
  SCM_SETCAR (smob, smob_tag_);
  SCM_SETCDR (smob, this);
  return smob;
}

SCM
Atom::make_atom (SCM outputfunc)
{
  Atom * a= new Atom(outputfunc);
  return a->make_smob ();
}

SCM
Atom::copy_self () const
{
  return (new Atom (*this))->make_smob ();
}

bool
Atom::Atom_b (SCM obj)
{
  return(SCM_NIMP(obj) && SCM_CAR(obj) == smob_tag_);
}

Atom* 
Atom::atom_l (SCM a)
{
  assert (Atom_b (a));
  return (Atom*) SCM_CDR(a);
}


ADD_GLOBAL_CTOR_WITHNAME(atomsmob, Atom::init_smob);
#endif


