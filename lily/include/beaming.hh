/*
  beaming.hh -- declare beaming.hh

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef BEAMING_HH
#define BEAMING_HH

#include "std-vector.hh"
#include "moment.hh"

struct Beaming_info
{
  Moment start_moment_;
  Drul_array<int> beam_count_drul_;

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
  vector<Beaming_info> infos_;

  int beam_extend_count (Direction) const;
  int best_splitpoint_index (Moment &beat_length, bool subdivide) const;
  void beamify (Moment &beat_length, bool subdivide);
  void add_stem (Moment d, int beams);
  void clip_edges ();
};

#endif /* BEAMING_HH */
