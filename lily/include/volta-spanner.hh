/*
  volta-spanner.hh -- part of GNU LilyPond

  (c) 1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef VOLTA_SPANNER_HH
#define VOLTA_SPANNER_HH


#include "pointer.hh"
#include "spanner.hh"

/** Volta bracket with number */

class Volta_spanner : public Spanner
{
public:
  Volta_spanner ();
 
  void add_column (Note_column*);
  void add_bar (Bar*);
 
  String number_str_;
  bool last_b_;

 
protected:
  virtual Molecule* do_brew_molecule_p () const;
  VIRTUAL_COPY_CONS (Score_element);

  virtual void do_add_processing ();
  static  Interval dim_callback (Dimension_cache const*);
  virtual void do_post_processing ();
};

#endif // VOLTA_SPANNER_HH

