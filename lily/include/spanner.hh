/*
  spanner.hh -- part of GNU LilyPond

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef SPANNER_HH
#define SPANNER_HH

#include "lily-proto.hh"
#include "score-element.hh"
#include "drul-array.hh"
#include "rod.hh"


class Axis_group_spanner;
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
class Spanner : public  Score_element {
  Drul_array<Item*> spanned_drul_;

public:
  Link_array<Spanner> broken_into_l_arr_;
  void set_bound (Direction d, Item*);
  Item *get_bound (Direction d) const;
  
  Spanner ();
  Spanner (Spanner const &);
  bool broken_b () const;
  void do_break ();

  static int compare (Spanner * const &,Spanner * const &);
  virtual Array<Rod> get_rods () const;
  virtual Array<Spring> get_springs () const;  
  virtual Score_element* find_broken_piece (Line_of_score*) const;
protected:
  void set_my_columns ();
  VIRTUAL_COPY_CONS(Score_element);
  Real get_broken_left_end_align () const;

  virtual void do_space_processing ();
  virtual void do_break_processing ();
  Real spanner_length () const;
  virtual Line_of_score*line_l () const;
};
#endif
