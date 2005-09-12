/*
  tie.hh -- declare Tie

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef TIE_HH
#define TIE_HH

#include "lily-guile.hh"
#include "lily-proto.hh"
#include "skyline.hh"



Interval
get_skyline_attachment (Drul_array< Array < Skyline_entry > > const &skylines,
			Real y1);

struct Tie_details
{
  Real height_limit_;
  Real ratio_;
  Real staff_space_;
  Real x_gap_;
  
  Tie_details ();
  void init (Grob *);
};
  
struct Tie_configuration
{
  int head_position_;
  int position_;
  
  Direction dir_;
  Interval attachment_x_;
  Real delta_y_;
  
  Tie_configuration ();
  void center_tie_vertically (Tie_details const &);
  Bezier get_bezier (Tie_details const &) const;
  Real height (Tie_details const&) const;
  
  static int compare (Tie_configuration const &a,
		      Tie_configuration const &b);
  static Real distance (Tie_configuration const &a,
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
  static int get_position (Grob *);
  static Direction get_default_dir (Grob *);
  static void get_configuration (Grob *, Grob *, Tie_configuration *,
				 Drul_array< Array<Skyline_entry> > const *,
				 Tie_details const & 
				 );
  static void set_control_points (Grob *, Grob *,Tie_configuration const&,
				  Tie_details const&
				  );
  static void set_default_control_points (Grob *);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (set_spacing_rods, (SCM));
  static int compare (Grob *const &s1,
		      Grob *const &s2);

  static Interval get_default_attachments (Spanner *me, Grob *common, Real gap,
					   int *staff_position, bool *in_between);
  
};


#endif // TIE_HH
