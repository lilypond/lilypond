/*   
  separating-group-spanner.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SEPARATING_GROUP_SPANNER_HH
#define SEPARATING_GROUP_SPANNER_HH

#include "spanner.hh"


class Separating_group_spanner : public Spanner
{
  Link_array<Single_malt_grouping_item> spacing_unit_l_arr_;
public:
  DECLARE_MY_RUNTIME_TYPEINFO;
  void add_spacing_unit (Single_malt_grouping_item*);
protected:
  SCORE_ELEMENT_CLONE (Separating_group_spanner);
  virtual Array<Rod> get_rods () const;
  virtual void do_substitute_dependency (Score_element*,Score_element*);
};

#endif /* SEPARATING_GROUP_SPANNER_HH */

