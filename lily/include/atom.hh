/*
  atom.hh -- declare Atom

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef ATOM_HH
#define ATOM_HH

#include "protected-scm.hh"
#include "string.hh"
#include "box.hh"
#include "lily-proto.hh"
#include "lily-guile.hh"


#ifdef ATOM_SMOB

/// a symbol which can be translated, and freely copied
class Atom {
  static long smob_tag_;

  static SCM smob_mark (SCM);
  static scm_sizet smob_free (SCM);
  static int smob_display (SCM, SCM, scm_print_state*);
  SCM make_smob () const;
public:
  Offset off_;
  Atom (SCM s);

  static SCM make_atom (SCM outputfunc);
  SCM copy_self () const;
  static Atom *atom_l (SCM);

  /// Is #obj# a Foo?
  static bool Atom_b(SCM obj);
  static void init_smob ();
  
  SCM func_;
  SCM font_;
};

#else

class Atom {
public:
  Atom (SCM s);
  Offset off_;
  /*
    SCM expression that (when evaluated) gives a TeX string
representing a musical notation symbol.  */
  Protected_scm func_;
  Protected_scm font_;
};

#endif

#endif
