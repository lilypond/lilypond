/*
  paper-column.hh -- declare  Paper_column

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef PAPER_COLUMN_HH
#define PAPER_COLUMN_HH

#include "item.hh"
#include "rod.hh"

class Paper_column : public Item
{
  int rank_;
  /// if lines are broken then this column is in #line#
  System *system_;

  // ugh: friend declarations.
  friend void set_loose_columns (System *which, Column_x_positions const *posns);
  friend class System;
public:
  Paper_column (SCM);
  Paper_column (Paper_column const &);

  virtual Grob *clone () const;
  virtual void do_break_processing ();
  virtual Paper_column *get_column () const;
  virtual System *get_system () const;
  void set_system (System *);

  static int compare (Grob * const &a,
		      Grob * const &b);
  static bool less_than (Grob *const &a,
			 Grob *const &b);

  int get_rank () const { return rank_; }
  void set_rank (int);

  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM));

  DECLARE_GROB_INTERFACE();
  static int get_rank (Grob const *);
  static bool is_musical (Grob *);
  static Moment when_mom (Grob *);
  static bool is_used (Grob *);
  static bool is_breakable (Grob *);
  static Real minimum_distance (Grob *l, Grob *r);
  static Interval break_align_width (Grob *me);
};

#endif // PAPER_COLUMN_HH

