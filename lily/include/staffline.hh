/*
  staffline.hh -- horizontal structures for broken scores.

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef STAFFLINE_HH
#define STAFFLINE_HH

#include "spanner-elem-group.hh"

/// one broken line of staff.
struct Line_of_staff : public Spanner_elem_group{

    SCORE_ELEM_CLONE(Line_of_staff);
public:
    DECLARE_MY_RUNTIME_TYPEINFO;

    /* *************** */
    /** 
      Add an element. If it is a Element_group, only the dependency
      (otherwise, might translate doubly) */
    void add_element(Score_elem*);
};

#endif
