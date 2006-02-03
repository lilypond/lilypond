/*
  break-align-interface.hh -- declare Break_align_interface

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef BREAK_ALIGN_INTERFACE_HH
#define BREAK_ALIGN_INTERFACE_HH

#include "item.hh"

class Break_align_interface
{
public:
  static Link_array__Grob_ ordered_elements (Grob *me);
  static bool has_interface (Grob *);
  static void add_element (Grob *me, Grob *add);
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM element));
  DECLARE_SCHEME_CALLBACK (self_align_callback, (SCM element));
};
struct Break_aligned_interface
{
  static bool has_interface (Grob *);
};

#endif // BREAK_ALIGN_INTERFACE_HH
