
/*
  line-of-score.hh -- part of GNU LilyPond

  (c) 1996--2007 Han-Wen Nienhuys
*/

#ifndef SYSTEM_HH
#define SYSTEM_HH

#include "column-x-positions.hh"
#include "spanner.hh"
#include "grob-array.hh"

/*
  If you keep following offset reference points, you will always end
  up at the root object. This root object is called @ref{System}, and it
  represents a system (i.e. a line of music).
*/
class System : public Spanner
{
  int rank_;
  Grob_array *all_elements_;
  void init_elements ();
  friend class Paper_score;	// ugh.
  Paper_score *pscore_;	// ugh.
  
public:
  Paper_score *paper_score () const;
  int get_rank () const;
  void post_processing ();
  SCM get_paper_system ();
  SCM get_paper_systems ();

  System (SCM, Object_key const *);
  System (System const &, int);

  int element_count () const;
  int spanner_count () const;

  void break_into_pieces (vector<Column_x_positions> const &);
  static bool has_interface (Grob *);

  vector<Item*> broken_col_range (Item const *, Item const *) const;
  vector<Grob*> columns () const;
  Grob *column (int i) const;

  void add_column (Paper_column *);
  void typeset_grob (Grob *);
  void pre_processing ();

protected:
  virtual void derived_mark () const;
  virtual Grob *clone (int count) const;
};

void set_loose_columns (System *which, Column_x_positions const *posns);

#endif /* SYSTEM_HH */

