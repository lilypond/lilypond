
/*   
  tie-column.hh -- declare Tie_column
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef TIE_COLUMN_HH
#define TIE_COLUMN_HH

#include "spanner.hh"

class Tie_column : public Spanner
{
public:
  VIRTUAL_COPY_CONS (Score_element);
  void add_tie (Tie*);
  Tie_column (SCM s);
protected:
  virtual void after_line_breaking ();
  void set_directions ();
};

#endif /* TIE_COLUMN_HH */

