/*   
  beaming.hh -- declare beaming.hh
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef BEAMING_HH
#define BEAMING_HH

#include "parray.hh"
#include "drul-array.hh"
#include "moment.hh"

struct Beaming_info
{
  Moment start_mom_;
  Drul_array<int> beams_i_drul_;

  Beaming_info (Moment, int);
  int count (Direction d);
  Beaming_info ();
};

/*
  Generate beaming given durations of notes. Beam uses this to
  set_beaming () for each of its stems.
*/
struct Beaming_info_list
{
  Array<Beaming_info> infos_;

  int beam_extend_count (Direction) const;
  int best_splitpoint_index (Moment &beat_length,bool subdivide) const;
  void beamify (Moment &beat_length,bool subdivide);
  void add_stem (Moment d, int beams);
};


#endif /* BEAMING_HH */
