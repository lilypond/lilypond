/*
  tie.hh -- declare Tie

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef TIE_HH
#define TIE_HH

#include "lily-guile.hh"
#include "lily-proto.hh"
#include "skyline.hh"


  
class Tie_configuration
{
public:
  int position_;
  Direction dir_;
  Real delta_y_;


  /* computed. */
  Interval attachment_x_;
  Grob *tie_;
  int head_position_;
  
  Tie_configuration ();
  void center_tie_vertically (Tie_details const &);
  Bezier get_transformed_bezier (Tie_details const &) const;
  Bezier get_untransformed_bezier (Tie_details const &) const;
  Real height (Tie_details const&) const;
  
  static int compare (Tie_configuration const &a,
		      Tie_configuration const &b);
  static Real distance (Tie_configuration const &a,
		       Tie_configuration const &b);
};

INSTANTIATE_COMPARE (Tie_configuration, Tie_configuration::compare);

class Ties_configuration
{
public:
  Array<Tie_configuration> ties_;
};

class Tie
{
public:
  static void set_head (Grob *, Direction, Grob *head);
  static bool has_interface (Grob *);
  static Grob *head (Grob *, Direction);
  static int get_column_rank (Grob *, Direction);
  static int get_position (Grob *);
  static Direction get_default_dir (Grob *);
  static void get_configuration (Grob *, Tie_configuration *,
				 Tie_formatting_problem const &);
  static void set_control_points (Grob *, Grob *,
				  Tie_configuration const&,
				  Tie_details const&);
  static void set_default_control_points (Grob *);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (set_spacing_rods, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_control_points, (SCM));
  static int compare (Grob *const &s1,
		      Grob *const &s2);

  static Interval get_default_attachments (Spanner *me, Grob *common, Real gap,
					   int *staff_position, bool *in_between,
					   Tie_details const &);
  
};


#endif // TIE_HH
