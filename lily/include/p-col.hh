/*
  p-col.hh -- declare  Paper_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef P_COL_HH
#define P_COL_HH

#include "horizontal-group-item.hh"
#include "plist.hh"
#include "rod.hh"


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

class Paper_column : public Horizontal_group_item { 
public:
  DECLARE_MY_RUNTIME_TYPEINFO;
  SCORE_ELEMENT_CLONE(Paper_column);
  Drul_array< Array<Column_rod> > minimal_dists_arr_drul_;

  void preprocess ();
  /// set a minimum distance
  void add_rod (Paper_column * to, Real distance);
  
  /** prebreak is put before end of line.
    if broken here, then (*this) column is discarded, and prebreak
    is put at end of line, owned by Col
    */
  Paper_column *prebreak_l() const;

  /// postbreak at beginning of the new line
  Paper_column *postbreak_l() const;

  virtual Paper_column * column_l () const;
  /// if lines are broken then this column is in #line#
  Line_of_score *line_l_;

  virtual Line_of_score *line_l () const;
  bool error_mark_b_;
  bool used_b_ ;		// manual override.. 
    

  /// which  one (left =0)
  int rank_i() const;

  /// does this column have items
  bool used_b() const;
  bool breakpoint_b() const;
    
  void add_item (Item *i);

  Paper_column();

  /**
    which col comes first?.
    signed compare on columns.

    @return < 0 if c1 < c2.
    */
  static int compare (const Paper_column &c1, const Paper_column &c2);
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


#include "compare.hh"
INSTANTIATE_COMPARE(Paper_column &, Paper_column::compare);
     
#endif // P_COL_HH

