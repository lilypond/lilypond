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
  Real edge_y_;
  
  Tie_configuration ()
  {
    dir_ = CENTER;
    position_ = 0;
  }
  
  static int compare (Tie_configuration const &a,
		      Tie_configuration const &b);
};

class Tie
{
public:
  static void set_head (Grob *, Direction, Grob *head);
  static void set_interface (Grob *);
  static bool has_interface (Grob *);
  static void set_direction (Grob *);
  static Grob *head (Grob *, Direction);
  static int get_column_rank (Grob *, Direction);
  static Real get_position (Grob *);
  static Direction get_default_dir (Grob *);
  static SCM get_control_points (SCM);
  static SCM get_configuration (SCM);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (set_spacing_rods, (SCM));
};


#endif // TIE_HH
