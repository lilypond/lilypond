/*
  vertical-group-spanner.hh -- declare Vertical_group_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SPAN_VERTICAL_GROUP_HH
#define SPAN_VERTICAL_GROUP_HH

#include "axis-group-spanner.hh"
#include "vertical-group-elem.hh"

/** An element which groups a line. 
 */
class Vertical_group_spanner : public Axis_group_spanner, public Vertical_group_element
{
protected:
  SCORE_ELEM_CLONE(Vertical_group_spanner);
  virtual void remove_all() { Vertical_group_element::remove_all (); }
  virtual void do_junk_links () { Axis_group_spanner::do_junk_links (); }
  virtual void do_unlink () { Axis_group_spanner::do_unlink (); }
public:
  DECLARE_MY_RUNTIME_TYPEINFO;
  virtual void add_element (Graphical_element*e) { Vertical_group_element::add_element (e); }
  virtual void remove_element (Graphical_element*e) { Vertical_group_element::remove_element (e); }  
};


#endif // SPAN_VERTICAL_GROUP_HH
