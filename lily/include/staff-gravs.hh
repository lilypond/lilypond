/*
  staff-gravs.hh -- declare Line_group_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STAFF_GRAVS_HH
#define STAFF_GRAVS_HH

#include "engraver-group.hh"

/**
  Engravers which manage a Staff (one 5-line linestaff)
  */
class Line_group_engraver : public Engraver{
    Line_of_staff *staffline_p_;   
    Link_array<Score_elem> staff_elem_l_arr_;

protected:
    virtual void do_creation_processing();
    virtual void do_removal_processing();
    virtual void acknowledge_element(Score_elem_info);

public:
    NAME_MEMBERS();
    Line_group_engraver();
};

#endif // STAFF_GRAVS_HH
