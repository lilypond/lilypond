/*   
  separating-group-spanner.hh -- declare Separating_group_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SEPARATING_GROUP_SPANNER_HH
#define SEPARATING_GROUP_SPANNER_HH

#include "spanner.hh"


class Separating_group_spanner : public Spanner
{
  Link_array<Single_malt_grouping_item> spacing_unit_l_arr_;
public:
  Separating_group_spanner ();
  void add_spacing_unit (Single_malt_grouping_item*);
protected:
  VIRTUAL_COPY_CONS(Score_element);
  virtual Array<Rod> get_rods () const;
  virtual void do_substitute_element_pointer (Score_element*,Score_element*);
};

#endif /* SEPARATING_GROUP_SPANNER_HH */

