/*   
  multi-measure-rest.hh -- declare Multi_measure_rest
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#ifndef MULTI_MEASURE_REST_HH
#define MULTI_MEASURE_REST_HH

#include "lily-proto.hh"
#include "lily-guile.hh"
#include "rod.hh"
/*
  properties:

  columns -- list of paper-columns
 */
class Multi_measure_rest
{
public:
  static void set_interface (Score_element*);
  static bool has_interface (Score_element*);
  static SCM brew_molecule (SCM);
  static  void add_column (Score_element*,Item*);
  static SCM set_spacing_rods (SCM);
};

#endif /* MULTI_MEASURE_REST_HH */

