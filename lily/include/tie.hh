/*
  tie.hh -- declare Tie

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TIE_HH
#define TIE_HH

#include "spanner.hh"
#include "rod.hh"

/**
  Connect two noteheads.
  */
class Tie : public Spanner
{
public:
  Tie (SCM);
  static void set_head (Score_element*,Direction, Item*head_l);
  static void set_interface (Score_element*);
  VIRTUAL_COPY_CONS(Score_element);
  static Rhythmic_head* head (Score_element*,Direction) ;
  static Real position_f (Score_element*) ;
  static SCM brew_molecule (SCM);
  static Direction get_default_dir(Score_element*) ;
  static SCM after_line_breaking (SCM);


  /*
    JUNKME
   */
  Array<Offset> get_encompass_offset_arr () const;
  Bezier get_curve () const;
  Drul_array<Real> dy_f_drul_;
  Drul_array<Real> dx_f_drul_;
  virtual Array<Rod> get_rods () const;
  Array<Offset> get_controls () const;
};

#endif // TIE_HH
