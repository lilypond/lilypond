/*
  break-align-interface.hh -- declare Break_align_interface

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef BREAK_ALIGN_INTERFACE_HH
#define BREAK_ALIGN_INTERFACE_HH

#include "item.hh"

class Break_align_interface
{
public:
  static void do_alignment (Grob*);
  static void new_do_alignment (Grob*);  
  static void set_interface (Grob*);
  static bool has_interface (Grob*);
  static void add_element (Grob*me, Grob*add);
  DECLARE_SCHEME_CALLBACK (alignment_callback, (SCM element, SCM axis));
  DECLARE_SCHEME_CALLBACK (self_align_callback, (SCM element, SCM axis));
  
};
struct Break_aligned_interface
{
  static bool has_interface (Grob*);
};


#endif // BREAK_ALIGN_INTERFACE_HH
