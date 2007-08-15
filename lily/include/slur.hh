/*
  slur.hh -- declare Slur

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SLUR_HH
#define SLUR_HH

#include "grob-info.hh"
#include "lily-proto.hh"
#include "lily-guile.hh"
#include "std-vector.hh"

class Slur
{
public:
  static void add_column (Grob *me, Grob *col);
  static void add_extra_encompass (Grob *me, Grob *col);
  static void replace_breakable_encompass_objects (Grob *me);
  static void auxiliary_acknowledge_extra_object (Grob_info, vector<Grob*>&, vector<Grob*>&);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_control_points, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (pure_height, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM));
  DECLARE_SCHEME_CALLBACK (outside_slur_callback, (SCM, SCM));
  DECLARE_SCHEME_CALLBACK (pure_outside_slur_callback, (SCM, SCM, SCM, SCM));
  static bool has_interface (Grob *);
  static Bezier get_curve (Grob *me);
};

#endif /* SLUR_HH */
