/*
  beaming.hh -- declare beaming.hh

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef BEAMING_HH
#define BEAMING_HH

#include "std-vector.hh"
#include "moment.hh"
#include "lily-proto.hh"

struct Beaming_info
{
  Moment start_moment_;
  Drul_array<int> beam_count_drul_;

  Beaming_info (Moment, int);
  Beaming_info ();

  int count (Direction d);
};

/*
  Generate beaming given durations of notes. Beam uses this to
  set_beaming () for each of its stems.
*/
struct Beaming_info_list
{
  vector<Beaming_info> infos_;
  Context *context_; 

  Beaming_info_list ();
  
  int beam_extend_count (Direction) const;
  int best_splitpoint_index (Moment, bool, bool *split) const;
  void beamify ();
  void beamify (Moment, bool);
  void add_stem (Moment d, int beams);
};

Beaming_info_list *make_beaming_info_list (Context *);

#endif /* BEAMING_HH */
