/*   
  multi-measure-rest.hh -- declare Multi_measure_rest
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#ifndef MULTI_MEASURE_REST_HH
#define MULTI_MEASURE_REST_HH

#include "item.hh"

class Multi_measure_rest : public Item 
{
public:
  Multi_measure_rest ();
  int measures_i_;

  

protected:
  virtual void do_print () const;
  virtual Molecule *brew_molecule_p () const;
};

#endif /* MULTI_MEASURE_REST_HH */

