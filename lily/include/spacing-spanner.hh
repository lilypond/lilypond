/*   
  spacing-spanner.hh -- declare Spacing_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SPACING_SPANNER_HH
#define SPACING_SPANNER_HH

#include "spanner.hh"

class Spacing_spanner
{
public:
  static void set_interface (Grob*);
  static void do_measure (Grob*,Link_array<Grob>) ;

  DECLARE_SCHEME_CALLBACK(set_springs, (SCM ));
  static Real stem_dir_correction (Grob*,Grob*,Grob*)  ;
  static Real default_bar_spacing (Grob*,Grob*,Grob*,Moment)  ;
  static Real note_spacing (Grob*,Grob*,Grob*,Moment)  ;
  static Real get_duration_space (Grob*,Moment dur, Moment shortest) ;
};

#endif /* SPACING_SPANNER_HH */

