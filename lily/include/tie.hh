/*
  tie.hh -- declare Tie

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TIE_HH
#define TIE_HH

#include "lily-guile.hh"
#include "lily-proto.hh"


/*
  heads -- pair of element pointers, pointing to the two heads of the
  tie.  */
class Tie
{
public:
  static void set_head (Score_element*,Direction, Item*head_l);
  static void set_interface (Score_element*);
  static bool has_interface (Score_element*);
  static Score_element * head (Score_element*,Direction) ;
  static Real position_f (Score_element*) ;
  static SCM brew_molecule (SCM);
  static Direction get_default_dir(Score_element*) ;
  static SCM get_control_points (SCM);
  static SCM set_spacing_rods (SCM);
};

#endif // TIE_HH
