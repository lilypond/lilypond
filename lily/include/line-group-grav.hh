/*
  line-group-grav.hh -- declare Line_group_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef LINE_GROUP_GRAV_HH
#define LINE_GROUP_GRAV_HH

#include "engraver.hh"
#include "lily-proto.hh"

/**
  Engravers put elements on the same or lowel level in a line
  */
class Line_group_engraver : public Engraver{
protected:

  Vertical_group_spanner *staffline_p_;   


  virtual void create_line_spanner ();
  virtual void do_creation_processing();
  virtual void do_removal_processing();
  virtual void acknowledge_element (Score_elem_info);

public:
  TRANSLATOR_CLONE(Line_group_engraver);
  DECLARE_MY_RUNTIME_TYPEINFO;
  Line_group_engraver();
};


#endif // LINE_GROUP_GRAV_HH

