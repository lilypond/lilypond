/*
  line-group-engraver.hh -- declare Line_group_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef LINE_GROUP_GRAV_HH
#define LINE_GROUP_GRAV_HH

#include "engraver-group.hh"
#include "lily-proto.hh"

/**
  Engravers put elements on the same or lowel level in a line
  */
class Line_group_engraver_group : public Engraver_group_engraver {
protected:
  Vertical_group_spanner *staffline_p_;   

  virtual void create_line_spanner ();
  virtual void do_creation_processing();
  virtual void do_removal_processing();
  virtual void typeset_element (Score_element*);
virtual void do_announces ();

public:
  VIRTUAL_COPY_CONS(Translator);
  
  Line_group_engraver_group();
};


#endif // LINE_GROUP_GRAV_HH

