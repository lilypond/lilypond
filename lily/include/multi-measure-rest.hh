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
 static SCM brew_molecule (SCM);
  

  void add_column (Item*);
  Molecule compound_rest (int)const;

  SCM member_brew_molecule () const;
  VIRTUAL_COPY_CONS (Score_element);

  SCM member_after_line_breaking ();
  static SCM after_line_breaking (SCM);
  virtual Array<Rod> get_rods () const;
};

#endif /* MULTI_MEASURE_REST_HH */

