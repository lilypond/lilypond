/*
  paper-column.hh -- declare  Paper_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef P_COL_HH
#define P_COL_HH

#include "item.hh"
#include "rod.hh"
#include "spring.hh"

class Paper_column : public Item
{ 
public:
  VIRTUAL_COPY_CONS (Grob);

  int  rank_i_;
  virtual void do_break_processing ();
  virtual Paper_column *column_l () const;
  virtual Line_of_score *line_l () const;

  /// if lines are broken then this column is in #line#
  Line_of_score *line_l_;

  static int rank_i (Grob*);

  DECLARE_SCHEME_CALLBACK(brew_molecule, (SCM));
  
  Paper_column (SCM);
  static bool musical_b (Grob *);
  static Moment when_mom (Grob*);

  static bool used_b (Grob*) ;
  void set_rank (int);
};
     
#endif // P_COL_HH

