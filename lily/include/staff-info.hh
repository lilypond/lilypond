/*
  staff-info.hh -- declare Staff_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef STAFF_INFO_HH
#define STAFF_INFO_HH

#include "lily-proto.hh"

/** struct to pass staff info along a Engraver hierarchy.

    JUNKME.
 */
struct Staff_info {
  Time_description const *time_C_;
  Rhythmic_grouping const *rhythmic_C_;
  Score_column *musical_l_;
  Score_column *command_l_;

  Paper_column * command_pcol_l();
  Paper_column * musical_pcol_l();
  Staff_info();
};

#endif // STAFF_INFO_HH
