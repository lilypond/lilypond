/*
  spanner.hh -- part of GNU LilyPond

  (c) 1996--1998 Han-Wen Nienhuys
*/

#ifndef SPANNER_HH
#define SPANNER_HH

#include "lily-proto.hh"
#include "score-element.hh"
#include "drul-array.hh"
#include "rod.hh"

/** A symbol which is attached between two columns. A spanner is a
  symbol which spans across several columns, so its final appearance
  can only be calculated after the breaking problem is solved.

  Examples

  \begin{itemize}
  \item (de)crescendo
  \item slur
  \item beam
  \item bracket
  \end{itemize}
  

  Spanner should know about the items which it should consider:
  e.g. slurs should be steep enough to "enclose" all those items. This
  is absolutely necessary for beams, since they have to adjust the
  length of stems of notes they encompass.

  */
class Spanner : public virtual Score_element {
public:
  Drul_array<Item*> spanned_drul_;
  void set_bounds (Direction d, Item*);

  
  Spanner ();
  Spanner (Spanner const &);
  bool broken_b () const;
  virtual Array<Rod> get_rods () const;
  Spanner* find_broken_piece (Line_of_score*) const;
protected:
  void set_my_columns ();
  VIRTUAL_COPY_CONS(Score_element);

  /**
    this is virtual; for instance, Line_of_score overrides it.
    */
  virtual void break_into_pieces ();

  Link_array<Spanner> broken_into_l_arr_;
  Spanner * unbroken_original_l_;

  virtual void do_unlink ();
  virtual void do_junk_links ();
  virtual void output_processing ();
  virtual void do_space_processing ();
  virtual void do_break_processing ();
  virtual Interval do_width () const;
  virtual void do_print () const;
  virtual Line_of_score*line_l () const;
};
#endif
