/*
  paper-column.hh -- declare  Paper_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef P_COL_HH
#define P_COL_HH

#include "item.hh"
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

class Paper_column : public Item
{ 
public:
  VIRTUAL_COPY_CONS(Score_element);

  /*
    ugh.

    TODO:
    
    * junk these after spacing is done.

    * Put these in Scheme.
   */
  
  Array<Column_rod>  minimal_dists_;
  Array<Column_spring > springs_;

  /// set a minimum distance
  void add_rod (Paper_column * to, Real distance);
  void add_spring (Paper_column * to, Real dist, Real strength);

  virtual Paper_column *column_l () const;
  virtual Line_of_score *line_l () const;

  /// if lines are broken then this column is in #line#
  Line_of_score *line_l_;

  /// which  one (left =0)
  int rank_i() const;

  Paper_column (SCM);
  Moment when_mom ()const;

  bool musical_b () const;
  bool used_b () const;
  void set_rank (int);
private:
    
  /**
    The ranking: left is smaller than right 
    -1 is uninitialised.
    */
  int rank_i_;

};
     
#endif // P_COL_HH

