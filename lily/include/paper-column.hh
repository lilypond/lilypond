/*
  paper-column.hh -- declare  Paper_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef P_COL_HH
#define P_COL_HH

#include "axis-group-item.hh"
#include "rod.hh"
#include "spring.hh"

/**
   stuff grouped vertically.
    This is a class to address items vertically. It contains the data for:
    \begin{itemize}
    \item
    unbroken score
    \item
    broken score
    \item
    the linespacing problem
    \end{itemize}
  */

class Paper_column : public Axis_group_item { 
public:
  VIRTUAL_COPY_CONS(Score_element);
  Drul_array<Array<Column_rod> > minimal_dists_arr_drul_;
  Drul_array<Array<Column_spring> > spring_arr_drul_;
  void preprocess ();
  /// set a minimum distance
  void add_rod (Paper_column * to, Real distance);
  void add_spring (Paper_column * to, Real dist, Real strength);

  virtual Paper_column * column_l () const;
  /// if lines are broken then this column is in #line#
  Line_of_score *line_l_;

  virtual Line_of_score *line_l () const;

  /// which  one (left =0)
  int rank_i() const;

  bool breakpoint_b() const;
  void add_item (Item *i);

  Paper_column();

  void set_rank (int);

  void OK() const;
  virtual void do_print() const;
private:
    
  /**
    The ranking: left is smaller than right 
    -1 is uninitialised.
    */
  int rank_i_;

};


// #include "compare.hh"
// INSTANTIATE_COMPARE(Paper_column &, Paper_column::compare);
     
#endif // P_COL_HH

