/*
  volta-spanner.hh -- part of GNU LilyPond

  (c) 1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef VOLTA_SPANNER_HH
#define VOLTA_SPANNER_HH

#include "text-def.hh"
#include "pointer.hh"
#include "spanner.hh"

/** Volta bracket with number */

class Volta_spanner : public Spanner
{
public:
  Volta_spanner ();
 
  void add_column (Note_column*);
  void add_column (Bar*);
 
  P<Text_def>  number_p_;
  Link_array<Bar> column_arr_;
  Link_array<Note_column> note_column_arr_;
  bool last_b_;
  bool visible_b_;
 
protected:
  virtual Molecule* do_brew_molecule_p () const;
  VIRTUAL_COPY_CONS (Score_element);

  virtual void do_add_processing ();
  virtual Interval do_height () const;
  virtual void do_post_processing ();
  virtual void do_substitute_element_pointer (Score_element*,Score_element*);
};

#endif // VOLTA_SPANNER_HH

