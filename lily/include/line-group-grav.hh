/*
  line-group-grav.hh -- declare Line_group_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef LINE_GROUP_GRAV_HH
#define LINE_GROUP_GRAV_HH

#include "engraver.hh"

/**
  Engravers put elements on the same or lowel level in a line
  */
class Line_group_engraver : public Engraver{
    Line_of_staff *staffline_p_;   

protected:
    virtual void do_creation_processing();
    virtual void do_removal_processing();
    virtual void acknowledge_element(Score_elem_info);

public:
    DECLARE_MY_RUNTIME_TYPEINFO;
    Line_group_engraver();
};


#endif // LINE_GROUP_GRAV_HH

