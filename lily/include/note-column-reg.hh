/*
  note-column-reg.hh -- declare Note_column_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef NOTE_COLUMN_REG_HH
#define NOTE_COLUMN_REG_HH

#include "register.hh"

class Note_column_register :public Request_register {
    Note_column *ncol_p_;
    bool h_shift_b_;
    int dir_i_;
    /* *************** */
    bool acceptable_elem_b(Staff_elem const*)const;
protected:
    virtual void set_feature(Feature);
    virtual void acknowledge_element(Staff_elem_info);
    virtual void pre_move_processing();

public:
    Note_column_register();
    NAME_MEMBERS(Note_column_register);
};
#endif // NOTE_COLUMN_REG_HH
