/*
  paper-system.hh -- declare Paper_system

  source file of the GNU LilyPond music typesetter

  (c) 2004--2006  Jan Nieuwenhuizen <janneke@gnu.org>
*/
#ifndef PAPER_SYSTEM_HH
#define PAPER_SYSTEM_HH

#include "stencil.hh"

/*
  A formatted "system" (A block of titling also is a Paper_system)

  To save memory, we don't keep around the System grobs, but put the
  formatted content of the grob is put into a
  Paper_system. Page-breaking handles Paper_system objects.
*/
class Paper_system
{
  DECLARE_SMOBS (Paper_system,);
  SCM mutable_property_alist_;
  SCM immutable_property_alist_;

  void init_vars ();
public:

  Paper_system (Stencil, SCM);
  SCM internal_get_property (SCM sym) const;
  void internal_set_property (SCM sym, SCM val);
  Real break_before_penalty () const;
};

DECLARE_UNSMOB (Paper_system, paper_system);

#endif /* PAPER_SYSTEM_HH */
