/*   
  spacing-spanner.hh -- declare Spacing_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SPACING_SPANNER_HH
#define SPACING_SPANNER_HH

#include "spanner.hh"

class Spacing_spanner
{
public:
  static void set_interface (Score_element*);
  static void do_measure (Score_element*,Link_array<Score_element>) ;

  static SCM set_springs (SCM);
  static Real stem_dir_correction (Score_element*,Score_element*,Score_element*)  ;
  static Real default_bar_spacing (Score_element*,Score_element*,Score_element*,Moment)  ;
  static Real note_spacing (Score_element*,Score_element*,Score_element*,Moment)  ;
  static Real get_duration_space (Score_element*,Moment dur, Moment shortest) ;
};

#endif /* SPACING_SPANNER_HH */

