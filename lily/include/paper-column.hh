/*
  paper-column.hh -- declare  Paper_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef P_COL_HH
#define P_COL_HH

#include "item.hh"
#include "rod.hh"


class Paper_column : public Item
{ 
public:
  VIRTUAL_COPY_CONS (Grob);

  static bool has_interface (Grob*);
  int  rank_;
  virtual void do_break_processing ();
  virtual Paper_column *get_column () const;
  virtual System *get_system () const;
  
  /// if lines are broken then this column is in #line#
  System *system_;

  static int get_rank (Grob*);

  DECLARE_SCHEME_CALLBACK(brew_molecule, (SCM));
  DECLARE_SCHEME_CALLBACK(before_line_breaking, (SCM));
  
  Paper_column (SCM);
  static bool musical_b (Grob *);
  static Moment when_mom (Grob*);

  static bool used_b (Grob*) ;
  void set_rank (int);
};
     
#endif // P_COL_HH

