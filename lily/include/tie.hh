/*
  tie.hh -- declare Tie

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef TIE_HH
#define TIE_HH

#include "lily-guile.hh"
#include "lily-proto.hh"


struct Tie_configuration
{
  int position_;
  Direction dir_;
  Interval attachment_x_;
  Real delta_y_;
  
  Tie_configuration ();
  
  static int compare (Tie_configuration const &a,
		      Tie_configuration const &b);
};
INSTANTIATE_COMPARE (Tie_configuration, Tie_configuration::compare);

class Tie
{
public:
  static void set_head (Grob *, Direction, Grob *head);
  static bool has_interface (Grob *);
  static void set_direction (Grob *);
  static Grob *head (Grob *, Direction);
  static int get_column_rank (Grob *, Direction);
  static Real get_position (Grob *);
  static Direction get_default_dir (Grob *);
  static void get_configuration (Grob *, Grob **, Tie_configuration *);
  static void set_control_points (Grob *, Grob **,Tie_configuration const&);
  static void set_default_control_points (Grob *);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (set_spacing_rods, (SCM));
  static int compare (Grob *const &s1,
		      Grob *const &s2);
  
};


#endif // TIE_HH
