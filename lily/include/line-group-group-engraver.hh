/*
  line-group-engraver.hh -- declare Line_group_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef LINE_GROUP_GRAV_HH
#define LINE_GROUP_GRAV_HH

#include "engraver-group-engraver.hh"
#include "lily-proto.hh"

/**
  Engravers put elements on the same or lowel level in a line.

  DEPRECATED.
  */
class Line_group_engraver_group : public Engraver_group_engraver {
protected:
  Spanner *staffline_p_;   

  virtual void create_line_spanner ();
  virtual void initialize();
  virtual void finalize();
  virtual void typeset_grob (Grob*);
public:
  VIRTUAL_COPY_CONS(Translator);
  
  Line_group_engraver_group();
};


#endif // LINE_GROUP_GRAV_HH

