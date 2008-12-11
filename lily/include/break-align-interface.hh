/*
  break-align-interface.hh -- declare Break_alignment_interface

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef BREAK_ALIGN_INTERFACE_HH
#define BREAK_ALIGN_INTERFACE_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

class Break_alignment_interface
{
public:
  static vector<Grob*> ordered_elements (Grob *me);
  DECLARE_GROB_INTERFACE();
  static void add_element (Grob *me, Grob *add);
  static SCM break_align_order (Item *me);
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM element));
  DECLARE_SCHEME_CALLBACK (self_align_callback, (SCM element));
};

struct Break_aligned_interface
{
  DECLARE_SCHEME_CALLBACK (calc_average_anchor, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_extent_aligned_anchor, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_break_visibility, (SCM));
  DECLARE_GROB_INTERFACE();
};

struct Break_alignable_interface
{
  DECLARE_SCHEME_CALLBACK (self_align_callback, (SCM element));
  DECLARE_GROB_INTERFACE();
};

#endif // BREAK_ALIGN_INTERFACE_HH
