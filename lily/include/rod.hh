/*   
  rod.hh -- declare Rod, Column_rod
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef ROD_HH
#define ROD_HH

#include "lily-proto.hh"
#include "direction.hh"
#include "drul-array.hh"

struct Rod
{
  Drul_array <Item*> item_l_drul_;
  Real distance_;

  /**
    translate the rod so as to refer to Paper_columns  
   */
  void columnize ();
  
  void add_to_cols ();

  Rod ();
};

#endif /* ROD_HH */

