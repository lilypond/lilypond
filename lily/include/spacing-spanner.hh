/*   
  spacing-spanner.hh -- declare Spacing_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SPACING_SPANNER_HH
#define SPACING_SPANNER_HH

#include "spanner.hh"

/**
   Read-only properties

   maximum-duration-for-spacing -- rational: space as if a duration of
     this type is available in this measure.


   
   Read properties from paper-column

   dir-list -- list of stem directions

   shortest-playing-duration -- duration of the shortest playing in that column.

   shortest-starter-duration -- duration of the shortest notes that starts
     exactly in that column.

   contains-grace -- boolean. Used to widen entries for grace notes.

   extra-space --  pair of distances

   stretch-distance -- pair of distances
 */
class Spacing_spanner
{
public:
  static void set_interface (Score_element*);
  static void do_measure (Score_element*,Link_array<Score_element>) ;

  DECLARE_SCHEME_CALLBACK(set_springs, (SCM ));
  static Real stem_dir_correction (Score_element*,Score_element*,Score_element*)  ;
  static Real default_bar_spacing (Score_element*,Score_element*,Score_element*,Moment)  ;
  static Real note_spacing (Score_element*,Score_element*,Score_element*,Moment)  ;
  static Real get_duration_space (Score_element*,Moment dur, Moment shortest) ;
};

#endif /* SPACING_SPANNER_HH */

