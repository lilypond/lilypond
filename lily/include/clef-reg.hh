/*
  clef.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef CLEF_HH
#define CLEF_HH

#include "scalar.hh"
#include "varray.hh"
#include "register.hh"

/// where is c-0 in the staff?
class Clef_register : public  Request_register {
    Clef_item *clef_p_;
    Clef_change_req * clef_req_l_;
    void create_clef();
    void read_req(Clef_change_req*);
    bool set_type(String);
protected:
     virtual void process_requests();
    virtual void pre_move_processing();
    virtual void post_move_processing();
    virtual bool try_request(Request*);
    virtual void acknowledge_element(Staff_elem_info);
public:
    int c0_position_i_;
    String clef_type_str_;

    /* ************** */
   
    Clef_register();
    NAME_MEMBERS(Clef_register);
   
};
#endif // CLEF_HH

