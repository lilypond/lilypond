/*
  staffline.hh -- horizontal structures for broken scores.

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef STAFFLINE_HH
#define STAFFLINE_HH

#include "spanner-elem-group.hh"

/// one broken line of staff.
struct Line_of_staff : public Spanner_elem_group{

    SPANNER_CLONE(Line_of_staff)
public:
    NAME_MEMBERS();

    /* *************** */
    /** 
      Add an element. If it is a Element_group, only the dependency
      (otherwise, might translate doubly) */
    void add_element(Score_elem*);
};

#endif
