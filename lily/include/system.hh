
/*
  line-of-score.hh -- part of GNU LilyPond

  (c) 1996--2002 Han-Wen Nienhuys
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
  void post_processing (bool);

  System (SCM);
  /// is #c# contained in #*this#?
  bool contains_b (Paper_column const *c) const;
  int element_count () const;
  int spanner_count () const;


  void break_into_pieces (Array<Column_x_positions> const&);
  void output_lines ();
  static bool has_interface (Grob*);
  
  Link_array<Item> broken_col_range (Item const*, Item const*) const;
  Link_array<Grob> columns () const;
  
  void add_column (Paper_column*);
  void typeset_grob (Grob*);
  void output_molecule (SCM, Offset);
  void output_scheme (SCM);
  void pre_processing ();
protected:
  VIRTUAL_COPY_CONS (Grob);
};

#endif /* SYSTEM_HH */

