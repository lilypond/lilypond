/*
  staff-info.hh -- declare 

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STAFF_INFO_HH
#define STAFF_INFO_HH

/// struct to pass staff info along a Request_register hierarchy.
struct Staff_info {
    int *c0_position_i_l_;
    Staff_walker *walk_l_;
    
    
    /// when is now?
    Time_description const *time_C_;
    Rhythmic_grouping const *rhythmic_C_;
    bool break_allowed_b_;

    Moment when();
    Staff * staff_l();
    Score *score_l();
    PCol * command_pcol_l();
    PCol * musical_pcol_l();
    Staff_column *column_l();
    Score_column *musical_l();
    Score_column *command_l();
    Staff_info();
};

#endif // STAFF_INFO_HH
