/*
  volta-spanner.hh -- part of GNU LilyPond

  (c) 1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef VOLTA_SPANNER_HH
#define VOLTA_SPANNER_HH


#include "spanner.hh"

/** Volta bracket with number */

class Volta_spanner : public Spanner
{
public:
  Volta_spanner (SCM);
  static SCM brew_molecule (SCM);
  void add_column (Note_column*);
  void add_bar (Bar*);
 
  SCM member_brew_molecule () const;
  VIRTUAL_COPY_CONS (Score_element);


  SCM member_after_line_breaking ();
  static SCM after_line_breaking (SCM);
};

#endif // VOLTA_SPANNER_HH

