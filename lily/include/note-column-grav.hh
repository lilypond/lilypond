/*
  note-column-grav.hh -- declare Note_column_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef NOTE_COLUMN_GRAV_HH
#define NOTE_COLUMN_GRAV_HH

#include "engraver.hh"

class Note_column_engraver :public Engraver {
    Rest_column * rest_col_l();
    Note_column * note_col_l();
    
    Array< Script * >  script_l_arr_;
    Stem * stem_l_;
    Note_column *ncol_p_;
    Rest_column *restcol_p_;
    bool h_shift_b_;
    Direction dir_;
    

    bool acceptable_elem_b (Score_elem const*) const;
protected:
    virtual void set_feature (Feature);
    virtual void acknowledge_element (Score_elem_info);
    virtual void do_pre_move_processing();
    virtual void do_post_move_processing();
public:
    Note_column_engraver();
    DECLARE_MY_RUNTIME_TYPEINFO;
};
#endif // NOTE_COLUMN_GRAV_HH
