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
  static bool has_interface (Grob*);
  static void get_spacing_params (Grob*,Real*,Real*);
};

#endif /* STAFF_SPACING_HH */
