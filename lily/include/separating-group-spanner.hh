/*   
  separating-group-spanner.hh -- declare Separating_group_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SEPARATING_GROUP_SPANNER_HH
#define SEPARATING_GROUP_SPANNER_HH

#include "spanner.hh"

class Separating_group_spanner : public Spanner
{
public:
  void add_spacing_unit (Item*);
  Separating_group_spanner(SCM);
protected:
  VIRTUAL_COPY_CONS(Score_element);
  virtual Array<Rod> get_rods () const;
};

#endif /* SEPARATING_GROUP_SPANNER_HH */

