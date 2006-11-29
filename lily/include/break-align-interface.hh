/*
  break-align-interface.hh -- declare Break_alignment_interface

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef BREAK_ALIGN_INTERFACE_HH
#define BREAK_ALIGN_INTERFACE_HH

#include "item.hh"

class Break_alignment_interface
{
public:
  static vector<Grob*> ordered_elements (Grob *me);
  static bool has_interface (Grob *);
  static void add_element (Grob *me, Grob *add);
  static SCM break_align_order (Item *me);
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM element));
  DECLARE_SCHEME_CALLBACK (self_align_callback, (SCM element));
};

struct Break_aligned_interface
{
  static bool has_interface (Grob *);
};

struct Break_alignable_interface
{
  DECLARE_SCHEME_CALLBACK (self_align_callback, (SCM element));
  static bool has_interface (Grob *);
};

#endif // BREAK_ALIGN_INTERFACE_HH
