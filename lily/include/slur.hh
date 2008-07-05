/*
  slur.hh -- declare Slur

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SLUR_HH
#define SLUR_HH

#include "lily-proto.hh"
#include "std-vector.hh"
#include "grob-interface.hh"

class Slur
{
public:
  static void add_column (Grob *me, Grob *col);
  static void add_extra_encompass (Grob *me, Grob *col);
  static void replace_breakable_encompass_objects (Grob *me);
  static void auxiliary_acknowledge_extra_object (Grob_info const &, vector<Grob*>&, vector<Grob*>&);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_control_points, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (pure_height, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM));
  DECLARE_SCHEME_CALLBACK (outside_slur_callback, (SCM, SCM));
  DECLARE_SCHEME_CALLBACK (pure_outside_slur_callback, (SCM, SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (outside_slur_cross_staff, (SCM, SCM));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));
  DECLARE_GROB_INTERFACE();
  static Bezier get_curve (Grob *me);
};

#endif /* SLUR_HH */
