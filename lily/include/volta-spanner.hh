/*
  volta-spanner.hh -- part of GNU LilyPond

  (c) 1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef VOLTA_SPANNER_HH
#define VOLTA_SPANNER_HH


#include "pointer.hh"
#include "spanner.hh"

/** Volta bracket with number */

class Volta_spanner : public Spanner
{
public:
  Volta_spanner (SCM);
  static SCM scheme_molecule (SCM);
  void add_column (Note_column*);
  void add_bar (Bar*);
 
protected:
  virtual Molecule do_brew_molecule () const;
  VIRTUAL_COPY_CONS (Score_element);

  virtual void do_add_processing ();
  virtual void after_line_breaking ();
};

#endif // VOLTA_SPANNER_HH

