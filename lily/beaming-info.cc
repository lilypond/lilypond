/*
  beaming-info.cc -- implement Beaming_info, Beaming_info_list

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "beaming.hh"

Beaming_info::Beaming_info ()
{
  start_moment_ = 0;
  beam_count_drul_[LEFT] = 0;
  beam_count_drul_[RIGHT] = 0;
}

Beaming_info::Beaming_info (Moment m, int i)
{
  start_moment_ = m;
  beam_count_drul_[LEFT] = i;
  beam_count_drul_[RIGHT] = i;
}

const int at_beat = 1 << 15;	//  WTF is this.

int
Beaming_info_list::best_splitpoint_index (Moment &beat_length,
					  bool subdivide) const
{
  int min_denominator = INT_MAX;
  int min_index = -1;
  Moment beat_pos;

  for (vsize i = 1; i < infos_.size (); i++)
    {
      beat_pos = infos_[i].start_moment_ / beat_length;
      int den = beat_pos.den ();
      if (infos_[i].beam_count_drul_[LEFT] == infos_[i - 1].beam_count_drul_[RIGHT]
	  && !subdivide)
	den *= 2;
      
      if (den < min_denominator)
	{
	  min_index = i;
	  min_denominator = den;
	}
    }

  return min_index | (min_denominator == 1 && subdivide ? at_beat : 0);
}

int
Beaming_info_list::beam_extend_count (Direction d) const
{
  if (infos_.size () == 1)
    return infos_[0].beam_count_drul_[d];

  Beaming_info thisbeam = boundary (infos_, d, 0);
  Beaming_info next = boundary (infos_, d, 1);

  return min (thisbeam.beam_count_drul_[-d], next.beam_count_drul_[d]);
}

void
Beaming_info_list::beamify (Moment &beat_length, bool subdivide)
{
  if (infos_.size () <= 1)
    return;

  Drul_array<Beaming_info_list> splits;

  int m = best_splitpoint_index (beat_length, subdivide);
  bool split = subdivide && (m & at_beat);
  m = m & ~at_beat;

  splits[LEFT].infos_ = vector<Beaming_info> (infos_.begin (),
					      infos_.begin () + m);
  splits[RIGHT].infos_ = vector<Beaming_info> (infos_.begin () + m,
					       infos_.end ());

  Direction d = LEFT;

  do
    {
      splits[d].beamify (beat_length, subdivide);
    }
  while (flip (&d) != LEFT);

  int middle_beams = (split ? 1
		      : min (splits[RIGHT].beam_extend_count (LEFT),
			     splits[LEFT].beam_extend_count (RIGHT)));

  do
    {
      if (splits[d].infos_.size () != 1)
	boundary (splits[d].infos_, -d, 0).beam_count_drul_[-d] = middle_beams;
    }
  while (flip (&d) != LEFT);

  infos_ = splits[LEFT].infos_;
  infos_.insert (infos_.end (),
		 splits[RIGHT].infos_.begin (),
		 splits[RIGHT].infos_.end ());

  clip_edges ();
}

void
Beaming_info_list::add_stem (Moment m, int b)
{
  infos_.push_back (Beaming_info (m, b));
}

void
Beaming_info_list::clip_edges ()
{
  if (infos_.size ())
    {
      infos_[0].beam_count_drul_[LEFT] = 0;
      infos_.back ().beam_count_drul_[RIGHT] = 0;
    }
}
