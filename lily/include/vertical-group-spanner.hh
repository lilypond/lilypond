/*
  vertical-group-spanner.hh -- declare Vertical_group_spanner

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SPAN_VERTICAL_GROUP_HH
#define SPAN_VERTICAL_GROUP_HH

#include "axis-group-spanner.hh"
#include "elem-group.hh"

/** An element which groups a line. 
 */
class Vertical_group_spanner : public Axis_group_spanner, public Vertical_group_element
{
protected:
    SCORE_ELEM_CLONE(Vertical_group_spanner);
    virtual void remove_all() { Vertical_group_element::remove_all (); }
public:
    DECLARE_MY_RUNTIME_TYPEINFO;
    virtual void add_element (Score_elem*e) { Vertical_group_element::add_element (e); }
    virtual void remove_element (Score_elem*e) { Vertical_group_element::remove_element (e); }
  
};

#endif // SPAN_VERTICAL_GROUP_HH
