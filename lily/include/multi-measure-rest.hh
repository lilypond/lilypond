/*   
  multi-measure-rest.hh -- declare Multi_measure_rest
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#ifndef MULTI_MEASURE_REST_HH
#define MULTI_MEASURE_REST_HH

#include "spanner.hh"


class Multi_measure_rest : public Spanner
{
public:
  Multi_measure_rest (SCM);
  
  static void set_interface (Score_element*);
  static bool has_interface (Score_element*);
  static SCM brew_molecule (SCM);
  static  void add_column (Score_element*,Item*);
  VIRTUAL_COPY_CONS (Score_element);
  virtual Array<Rod> get_rods () const;
};

#endif /* MULTI_MEASURE_REST_HH */

