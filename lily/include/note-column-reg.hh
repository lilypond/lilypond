/*
  note-column-reg.hh -- declare Note_column_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef NOTE_COLUMN_REG_HH
#define NOTE_COLUMN_REG_HH

#include "register.hh"

class Note_column_register :public Request_register {
    Rest_column * rest_col_l();
    Note_column * note_col_l();
    
    Array< Script * >  script_l_arr_;
    Stem * stem_l_;
    Note_column *ncol_p_;
    Rest_column *restcol_p_;
    bool h_shift_b_;
    int dir_i_;
    
    /* *************** */
    bool acceptable_elem_b(Score_elem const*)const;
protected:
    virtual void set_feature(Feature);
    virtual void acknowledge_element(Score_elem_info);
    virtual void pre_move_processing();
    virtual void post_move_processing();
public:
    Note_column_register();
    NAME_MEMBERS(Note_column_register);
};
#endif // NOTE_COLUMN_REG_HH
