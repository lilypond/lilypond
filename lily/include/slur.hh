/*
  slur.hh -- declare Slur

  (c) 1996--2002 Han-Wen Nienhuys
*/

#ifndef SLUR_HH
#define SLUR_HH

#include "lily-guile.hh"
#include "lily-proto.hh"
#include "rod.hh"

class Slur
{
public:
  static void add_column (Grob *me, Grob *col);
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM));
  static void set_interface (Grob*);
  static bool  has_interface (Grob*);
  static Array<Offset> get_encompass_offsets (Grob *me);
  static Bezier get_curve (Grob *me);
  static Direction get_default_dir (Grob *me);
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM));

  DECLARE_SCHEME_CALLBACK (height, (SCM,SCM));
private:  
  static Real get_first_notecolumn_y (Grob *me, Direction dir);
  static Offset broken_trend_offset (Grob *me, Direction dir);
  static Offset get_attachment (Grob *me,Direction dir, Grob **common);
  static void de_uglyfy (Grob *me,Slur_bezier_bow* bb, Real default_height);
  static SCM set_extremities (Grob *me);
  static void set_control_points (Grob *me);
  static void check_slope (Grob *me);
  static Offset encompass_offset (Grob *me, Grob *col, Grob **common);
};

#endif // SLUR_HH


