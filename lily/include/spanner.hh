/*
  spanner.hh -- part of GNU LilyPond

  (c) 1996--2004 Han-Wen Nienhuys
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
  int break_index_;
  
public:
  DECLARE_SCHEME_CALLBACK (set_spacing_rods, (SCM));
  
  Link_array<Spanner> broken_intos_;

  int get_break_index () const;
  // todo: move to somewhere else.
  Real get_broken_left_end_align () const;
  void substitute_one_mutable_property (SCM sym, SCM val)  ;
  bool fast_fubstitute_grob_list (SCM sym, SCM grob_list);
  // TODO: make virtual and do this for Items as well.
  Interval_t<int> spanned_rank_iv ();
  void set_bound (Direction d, Grob*);
  Item *get_bound (Direction d) const;
  
  Spanner (SCM);
  Spanner (Spanner const &);
  bool is_broken () const;
  void do_break ();
  Real spanner_length () const;

  static int compare (Spanner * const &,Spanner * const &);
  virtual Grob* find_broken_piece (System*) const;
  virtual SCM do_derived_mark () const;
  static bool has_interface (Grob*);
  virtual System *get_system () const;

protected:
  void set_my_columns ();

  VIRTUAL_COPY_CONSTRUCTOR (Grob, Spanner);
  virtual void do_break_processing ();
};

void add_bound_item (Spanner*, Grob*);

bool spanner_less ( Spanner *s1, Spanner* s2);
int broken_spanner_index (Spanner * sp);
#endif
