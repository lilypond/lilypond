/*
  script-grav.hh -- part of GNU LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SCRIPTGRAV_HH
#define SCRIPTGRAV_HH

#include "engraver.hh"


class Script_engraver : public Engraver {
    Array<Script *> script_p_arr_;
    Array<Script_req *> script_req_l_arr_;

    /* *************** */
    bool acceptable_elem_b(Score_elem*);
public:
    NAME_MEMBERS();
    Script_engraver();
protected:
    virtual bool do_try_request(Request*);
    virtual void do_process_requests();
    virtual void acknowledge_element(Score_elem_info);
    virtual void do_pre_move_processing();
    virtual void do_post_move_processing();

};

#endif // SCRIPTGRAV_HH
