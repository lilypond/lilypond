/*
  vertical-group-spanner.hh -- declare Vertical_group_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SPAN_VERTICAL_GROUP_HH
#define SPAN_VERTICAL_GROUP_HH

#include "axis-group-spanner.hh"
#include "vertical-group-element.hh"

/** An element which groups a line. 
 */
class Vertical_group_spanner : public Axis_group_spanner, public Vertical_group_element
{
protected:
  VIRTUAL_COPY_CONS(Score_element);
  virtual void do_junk_links () { Axis_group_spanner::do_junk_links (); }
  virtual void do_unlink () { Axis_group_spanner::do_unlink (); }
public:
  
  Vertical_group_spanner ();
};


#endif // SPAN_VERTICAL_GROUP_HH
