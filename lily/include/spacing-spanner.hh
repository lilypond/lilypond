/*   
  spacing-spanner.hh -- declare Spacing_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SPACING_SPANNER_HH
#define SPACING_SPANNER_HH

class Spacing_spanner : public Spanner
{
  Link_array<Paper_column> cols_;

  Spacing_spanner ();
protected:
  virtual void do_space_processing ();
};

#endif /* SPACING_SPANNER_HH */

