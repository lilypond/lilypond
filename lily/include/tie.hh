/*
  tie.hh -- declare Tie

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TIE_HH
#define TIE_HH

#include "lily-guile.hh"
#include "lily-proto.hh"


class Tie
{
public:
  static void set_head (Grob*,Direction, Grob*head);
  static void set_interface (Grob*);
  static bool has_interface (Grob*);
  static void set_direction (Grob*);
  static Grob * head (Grob*,Direction) ;
  static Real get_position (Grob*) ;
  static int get_column_rank (Grob*, Direction);
  static Direction get_default_dir (Grob*) ;
  static SCM get_control_points (SCM);
  DECLARE_SCHEME_CALLBACK (set_spacing_rods, (SCM ));
  DECLARE_SCHEME_CALLBACK (print, (SCM ));
};

#endif // TIE_HH
