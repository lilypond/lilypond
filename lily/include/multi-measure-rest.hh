/*   
  multi-measure-rest.hh -- declare Multi_measure_rest
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#ifndef MULTI_MEASURE_REST_HH
#define MULTI_MEASURE_REST_HH

#include "spanner.hh"
#include "staff-symbol-referencer.hh"

class Multi_measure_rest : public Spanner, public Staff_symbol_referencer
{
public:
  Multi_measure_rest ();
  int measures_i_;
  void add_column (Item*);

protected:
  virtual Molecule *do_brew_molecule_p () const;
  VIRTUAL_COPY_CONS (Score_element);

  virtual void do_add_processing ();
  virtual void do_post_processing ();
  virtual void do_print () const;
  virtual Array<Rod> get_rods () const;
};

#endif /* MULTI_MEASURE_REST_HH */

