/*   
  spacing-spanner.hh -- declare Spacing_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SPACING_SPANNER_HH
#define SPACING_SPANNER_HH

#include "spanner.hh"

class Spacing_spanner : public Spanner
{
public:
  Spacing_spanner ();
  VIRTUAL_COPY_CONS(Score_element);
  Score_column *scol (int) const;
  Array<Spring> do_measure (int,int) const;
  int col_count () const;
protected:
  virtual  Array<Spring> get_springs () const;

  Real stem_dir_correction (Score_column*,Score_column*)  const;
  Real default_bar_spacing (Score_column*,Score_column*,Moment)  const;
  Real note_spacing (Score_column*,Score_column*,Moment)  const;  
};

#endif /* SPACING_SPANNER_HH */

