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
 static SCM scheme_molecule (SCM);
  


protected:
  Molecule do_brew_molecule () const;
  virtual Array<Offset> get_encompass_offset_arr () const;
  Bezier get_curve () const;

  /*
    JUNKME
   */
  Drul_array<Real> dy_f_drul_;
  Drul_array<Real> dx_f_drul_;

  virtual Direction get_default_dir () const;
  virtual void after_line_breaking ();
  virtual void do_add_processing ();
  Array<Rod> get_rods () const;

private:  
  void de_uglyfy (Slur_bezier_bow* bb, Real default_height);
  void set_extremities ();
  void set_control_points ();
  int cross_staff_count () const;
  Offset encompass_offset (Note_column const* )const;
};

#endif // SLUR_HH


