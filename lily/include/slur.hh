/*
  slur.hh -- declare Slur

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef SLUR_HH
#define SLUR_HH

#include "spanner.hh"
#include "rod.hh"

/**
  A #Bow# which tries to drape itself around the stems too.
 */
class Slur : public Spanner
{
public:
  Slur (SCM);
  VIRTUAL_COPY_CONS(Score_element);

  void add_column (Note_column*);
 static SCM brew_molecule (SCM);
  
  Array<Offset> get_encompass_offset_arr () const;
  Bezier get_curve () const;

  Direction get_default_dir () const;
  static SCM after_line_breaking (SCM);
  Array<Rod> get_rods () const;
  Offset get_attachment (Direction dir, Score_element**common) const;

private:  
  void de_uglyfy (Slur_bezier_bow* bb, Real default_height);
  void set_extremities ();
  void set_control_points ();
  Offset encompass_offset (Score_element *col,Score_element**common)const;
};

#endif // SLUR_HH


