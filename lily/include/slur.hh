/*
  slur.hh -- declare Slur

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef SLUR_HH
#define SLUR_HH

#include "lily-guile.hh"
#include "lily-proto.hh"
#include "rod.hh"

class Slur
{
public:
  static void add_column (Score_element*me,Score_element*col);
  static SCM brew_molecule (SCM);
  static void set_interface (Score_element*);
  static bool  has_interface (Score_element*);
  static Array<Offset> get_encompass_offset_arr (Score_element*me) ;
  static Bezier get_curve (Score_element*me) ;
  static Direction get_default_dir (Score_element*me) ;
  static SCM after_line_breaking (SCM);
  static SCM set_spacing_rods (SCM);
private:  
  static Offset get_attachment (Score_element*me,Direction dir, Score_element**common) ;
  static void de_uglyfy (Score_element*me,Slur_bezier_bow* bb, Real default_height);
  static void set_extremities (Score_element*me);
  static void set_control_points (Score_element*me);
  static Offset encompass_offset (Score_element*me,Score_element *col,Score_element**common);
};

#endif // SLUR_HH


