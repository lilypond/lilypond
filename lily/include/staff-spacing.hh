/*   
staff-spacing.hh -- declare Staff_spacing

source file of the GNU LilyPond music typesetter

(c) 2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#ifndef STAFF_SPACING_HH
#define STAFF_SPACING_HH

#include "lily-proto.hh"

class Staff_spacing
{
public:
  static Real next_notes_correction (Grob*, Grob*);
  static Real next_note_correction (Grob*, Grob*, Interval);  
  static bool has_interface (Grob*);
  static void get_spacing_params (Grob*,Real*,Real*);

  static Interval bar_y_positions (Grob*);
  static  Grob*  extremal_break_aligned_grob (Grob*,Direction, Interval*);
};

#endif /* STAFF_SPACING_HH */
