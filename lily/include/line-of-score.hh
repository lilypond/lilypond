/*
  line-of-score.hh -- part of GNU LilyPond

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef SCORELINE_HH
#define SCORELINE_HH

#include "column-x-positions.hh"
#include "spanner.hh"

/// the columns of a score that form one line. FIXME: multiple inheritance
class Line_of_score : public Spanner
{
public:
/*
  imported the following  from Super_element
  
  The toplevel element. The Paper_score contains this element, and any
  element shoud be a dependency for the super element.

  It is the entry point for the "constraint solver"/ dependency
  tracker.  Every XXXX_processing () call traverses the entire
  dependency graph, and calls the appropriate
  Score_element::do_XXX_processing function on each Score_element it encounters.
  */
  
  void post_processing();
  void output_all ();

  int rank_i_;
  
  Line_of_score();
    
  /// is #c# contained in #*this#?
  bool contains_b (Paper_column const *c) const;
    
  static int compare (Line_of_score* const &,Line_of_score* const &);

  void break_into_pieces (Array<Column_x_positions> const&);
  void output_lines ();
  void output_line (bool last_line);
  void add_column (Paper_column*);
  
protected:
  VIRTUAL_COPY_CONS(Score_element);
};

#endif

