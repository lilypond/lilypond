/*
  staff-info.hh -- declare Staff_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STAFF_INFO_HH
#define STAFF_INFO_HH

#include "lily-proto.hh"

/// struct to pass staff info along a Engraver hierarchy.
struct Staff_info {
    int *c0_position_i_l_;
    Staff_symbol*staff_sym_l_;
    
    Time_description const *time_C_;
    Rhythmic_grouping const *rhythmic_C_;
    Score_column *musical_l_;
    Score_column *command_l_;

    ///
    bool break_allowed_b_;
    Score * score_l_;
    Score *score_l();
    Paper_column * command_pcol_l();
    Score_column* musical_l();
    Score_column *command_l();
    Paper_column * musical_pcol_l();
    Staff_info();
};

#endif // STAFF_INFO_HH
