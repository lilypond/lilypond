/*
  spanner.hh -- part of GNU LilyPond

  (c) 1996--2002 Han-Wen Nienhuys
*/

#ifndef SPANNER_HH
#define SPANNER_HH

#include "lily-proto.hh"
#include "grob.hh"
#include "drul-array.hh"
#include "rod.hh"

/** A symbol which is attached between two columns. A spanner is a
  symbol which spans across several columns, so its final appearance
  can only be calculated after the breaking problem is solved.

  Examples

  * (de)crescendo
  * slur
  * beam
  * bracket

  Spanner should know about the items which it should consider:
  e.g. slurs should be steep enough to "enclose" all those items. This
  is absolutely necessary for beams, since they have to adjust the
  length of stems of notes they encompass.

  */
class Spanner : public  Grob {
  Drul_array<Item*> spanned_drul_;

public:
  DECLARE_SCHEME_CALLBACK (set_spacing_rods, (SCM ));
  
  Link_array<Spanner> broken_into_l_arr_;

  // todo: move to somewhere else.
  Real get_broken_left_end_align () const;
  
  // TODO: make virtual and do this for Items as well.
  Interval_t<int> spanned_rank_iv ();
  void set_bound (Direction d, Grob*);
  Item *get_bound (Direction d) const;
  
  Spanner (SCM);
  Spanner (Spanner const &);
  bool broken_b () const;
  void do_break ();
  Real spanner_length () const;

  static int compare (Spanner * const &,Spanner * const &);
  virtual Grob* find_broken_piece (System*) const;
  virtual SCM do_derived_mark ();
protected:
  void set_my_columns ();

  VIRTUAL_COPY_CONS (Grob);
  virtual void do_break_processing ();
  virtual System*line_l () const;
};


void add_bound_item (Spanner*, Item* n);
void extend_spanner_over_elements (Grob*  span);


#endif
