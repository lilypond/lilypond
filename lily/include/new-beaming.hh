/*   
  new-beaming.hh -- declare New_beaming.hh
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef NEW_BEAMING_HH
#define NEW_BEAMING_HH

#include "parray.hh"
#include "drul-array.hh"
#include "moment.hh"

struct Beaming_info
{
  Moment start_mom_;
  Drul_array<int> beams_i_drul_;

  Beaming_info (Moment, int);
  int count  (Direction d);
  Beaming_info ();
};

struct Beaming_info_list
{
  Array<Beaming_info> infos_;

  int beam_extend_count (Direction) const;
  int min_denominator_index () const;
  void beamify ();
  void add_stem (Moment d, int beams);
};


#endif /* NEW_BEAMING_HH */

