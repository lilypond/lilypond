/*
  dynamic-grav.hh -- declare Dynamic_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef DYNAMIC_GRAV_HH
#define DYNAMIC_GRAV_HH

#include "engraver.hh"

class Dynamic_engraver : public Request_engraver {
    int dir_i_;
    Text_item * dynamic_p_;
    Crescendo * to_end_cresc_p_;
    Crescendo * cresc_p_;
    Span_dynamic_req * cresc_req_l_;
    Array<Dynamic_req*> dynamic_req_l_arr_;
    /* ************** */
public:
    Dynamic_engraver();
    ~Dynamic_engraver();
    NAME_MEMBERS();
protected:
    virtual void acknowledge_element(Score_elem_info);
    virtual bool do_try_request(Request *req_l);
    virtual void do_process_requests();
    virtual void do_pre_move_processing();
    virtual void do_post_move_processing();
    virtual void set_feature(Feature);
};

#endif // DYNAMIC_GRAV_HH
