/*
  paper-column.hh -- declare  Paper_column

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef PAPER_COLUMN_HH
#define PAPER_COLUMN_HH

#include "item.hh"
#include "rod.hh"


class Paper_column : public Item
{ 
public:
  int  rank_;
  /// if lines are broken then this column is in #line#
  System *system_;

  Paper_column (SCM, Object_key const*);
  Paper_column (Paper_column const&, int count);
  
  virtual Grob *clone (int count) const;

  static bool has_interface (Grob*);
  virtual void do_break_processing ();
  virtual Paper_column *get_column () const;
  virtual System *get_system () const;
  
  static int get_rank (Grob*);
  
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM));
  
  static bool is_musical (Grob *);
  static Moment when_mom (Grob*);

  static bool is_used (Grob*) ;
  void set_rank (int);
};
     
#endif // PAPER_COLUMN_HH

