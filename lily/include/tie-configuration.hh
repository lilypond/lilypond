/*
  tie-configuration.hh -- declare Tie_configuration

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef TIE_CONFIGURATION_HH
#define TIE_CONFIGURATION_HH

#include "lily-proto.hh"
#include "direction.hh"
#include "interval.hh"
#include "compare.hh"
#include "std-vector.hh"

class Tie_configuration
{
public:
  int position_;
  Direction dir_;
  Real delta_y_;

  /* computed. */
  Interval attachment_x_;
  
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

typedef std::vector<Tie_configuration> Ties_configuration;

#endif /* TIE_CONFIGURATION_HH */



