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
  Array<Spring> do_measure (Link_array<Paper_column>) const;

protected:
  virtual  Array<Spring> get_springs () const;

  Real stem_dir_correction (Paper_column*,Paper_column*)  const;
  Real default_bar_spacing (Paper_column*,Paper_column*,Moment)  const;
  Real note_spacing (Paper_column*,Paper_column*,Moment)  const;  
};

#endif /* SPACING_SPANNER_HH */

