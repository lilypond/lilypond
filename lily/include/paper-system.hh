/*
  paper-system.hh -- declare Paper_system

  source file of the GNU LilyPond music typesetter

  (c) 2004  Jan Nieuwenhuizen <janneke@gnu.org>
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
  DECLARE_SMOBS (Paper_system, );
  Stencil stencil_;
  bool is_title_;
public:
  Interval staff_refpoints_;
  Real penalty_;
  int number_;

  Paper_system (Stencil, bool);
  
  Stencil to_stencil () const;
  SCM stencils () const;
  bool is_title () const;
  Real penalty () const;
};

DECLARE_UNSMOB (Paper_system, paper_system);

#endif /* PAPER_SYSTEM_HH */
