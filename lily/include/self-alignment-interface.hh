/*   
  side-position-interface.hh -- declare Side_position_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SELF_ALIGNMENT_INTERFACE_HH
#define SELF_ALIGNMENT_INTERFACE_HH

#include "spanner.hh"

struct Self_alignment_interface
{
  static bool has_interface (Grob*);
  DECLARE_SCHEME_CALLBACK (aligned_on_self, (SCM element, SCM axis));
  DECLARE_SCHEME_CALLBACK (centered_on_parent, (SCM element, SCM axis));
  DECLARE_SCHEME_CALLBACK (centered_on_other_axis_parent, (SCM element, SCM axis));
};
#endif
