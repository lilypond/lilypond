
/*
  line-of-score.hh -- part of GNU LilyPond

  (c) 1996--2002 Han-Wen Nienhuys
*/

#ifndef SCORELINE_HH
#define SCORELINE_HH


#include "column-x-positions.hh"
#include "spanner.hh"

class System : public Spanner
{
public:
  int rank_i_;
  void post_processing (bool);

  System (SCM);
  /// is #c# contained in #*this#?
  bool contains_b (Paper_column const *c) const;
  int element_count () const;

  void break_into_pieces (Array<Column_x_positions> const&);
  void output_lines ();
  static bool has_interface (Grob*);
  
  Link_array<Item> broken_col_range (Item const*, Item const*) const;
  Link_array<Grob> column_l_arr () const;
  
  void add_column (Paper_column*);
  void typeset_grob (Grob*);
  void output_molecule (SCM, Offset);
  void output_scheme (SCM);
  void pre_processing ();
protected:
  VIRTUAL_COPY_CONS (Grob);
};

#endif

