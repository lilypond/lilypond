
/*
  line-of-score.hh -- part of GNU LilyPond

  (c) 1996--2005 Han-Wen Nienhuys
*/

#ifndef SYSTEM_HH
#define SYSTEM_HH


#include "column-x-positions.hh"
#include "spanner.hh"

/*
  If you keep following offset reference points, you will always end
up at the root object. This root object is called @ref{System}, and it
represents a system (i.e. a line of music).


 */
class System : public Spanner
{
public:
  int rank_;
  void post_processing ();
  SCM get_line ();
  SCM get_lines ();

  System (SCM, Object_key const*);
  System (System const&, int);
  virtual Grob *clone (int count) const;
  
  int element_count () const;
  int spanner_count () const;

  void break_into_pieces (Array<Column_x_positions> const&);
  static bool has_interface (Grob*);
  
  Link_array<Item> broken_col_range (Item const*, Item const*) const;
  Link_array<Grob> columns () const;
  
  void add_column (Paper_column*);
  void typeset_grob (Grob*);
  void pre_processing ();

protected:
};


void set_loose_columns (System* which, Column_x_positions const *posns);
#endif /* SYSTEM_HH */

