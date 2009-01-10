/*
  tie.hh -- declare Tie

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef TIE_HH
#define TIE_HH

#include "lily-proto.hh"
#include "skyline.hh"
#include "grob-interface.hh"


  

class Tie
{
public:
  static void set_head (Grob *, Direction, Grob *head);
  DECLARE_GROB_INTERFACE();
  static Grob *head (Grob *, Direction);
  static int get_column_rank (Grob *, Direction);
  static int get_position (Grob *);
  static Direction get_default_dir (Grob *);  
  static SCM get_control_points (Grob *, Grob *,
				 Tie_configuration const&,
				 Tie_details const&);
  static SCM get_default_control_points (Grob *);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (set_spacing_rods, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_control_points, (SCM));
  static bool less (Grob *const &s1,
		    Grob *const &s2);
};


#endif // TIE_HH
