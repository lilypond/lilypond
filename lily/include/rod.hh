/*   
  rod.hh -- declare Rod, Column_rod
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.ruu.nl>
  
 */

#ifndef ROD_HH
#define ROD_HH
#include "lily-proto.hh"
#include "direction.hh"
#include "drul-array.hh"

struct Column_rod {
  Paper_column *other_l_;
  Real distance_f_;

  Column_rod ();
  static int compare (const Column_rod &r1, const Column_rod &r2);
  void print () const;
};

struct Rod
{
  Drul_array <Item*> item_l_drul_;
  Real distance_f_;
  void add_to_cols ();

  Rod (Single_malt_grouping_item*,Single_malt_grouping_item*);
  Rod ();
};


#endif /* ROD_HH */

