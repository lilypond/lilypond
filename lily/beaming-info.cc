/*
  beaming-info.cc -- implement Beaming_info, Beaming_info_list

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "beaming.hh"
#include "context.hh"

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

int
Beaming_info_list::best_splitpoint_index (bool *at_boundary) const
{
  *at_boundary = true;
  for (vsize i = 1; i < infos_.size (); i++)  
    {
      if (infos_[i].group_start_  == infos_[i].start_moment_)
	return i;
    }

  for (vsize i = 1; i < infos_.size (); i++)  
    {
      if (infos_[i].beat_start_  == infos_[i].start_moment_)
	return i;
    }

  *at_boundary = false;
  
  int min_denominator = INT_MAX;
  int min_index = -1;
  
  Moment beat_pos;
  for (vsize i = 1; i < infos_.size (); i++)  
    {
      Moment dt = infos_[i].start_moment_ - infos_[i].beat_start_; 
      if (dt.den () < min_denominator)
	{
	  min_denominator = dt.den ();
	  min_index = i;
	}
    }

  return min_index;
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
Beaming_info_list::beamify (Context *context)
{
  if (infos_.size () <= 1)
    return;
  
  bool subdivide_beams = to_boolean (context->get_property ("subdivideBeams"));
  Moment beat_length = robust_scm2moment (context->get_property ("beatLength"), Moment (1, 4));
  
  SCM grouping = context->get_property ("beatGrouping");
  Moment measure_pos (0);
  
  vector<Moment> group_starts;
  vector<Moment> beat_starts;
  
  while (measure_pos <= infos_.back().start_moment_)
    {
      int count = 2;
      if (scm_is_pair (grouping))
	{
	  count = scm_to_int (scm_car (grouping));
	  grouping = scm_cdr (grouping);
	}

      group_starts.push_back (measure_pos);
      for (int i = 0; i < count; i++)
	{
	  beat_starts.push_back (measure_pos + beat_length * i);
	}
      measure_pos += beat_length * count;
    }
   
  vsize j = 0;
  vsize k = 0;
  for (vsize i = 0; i  < infos_.size(); i++)
    {
      while (j < group_starts.size()-1
	     && group_starts[j+1] <= infos_[i].start_moment_)
	j++;

      infos_[i].group_start_ = group_starts[j];

      while (k < beat_starts.size()-1
	     && beat_starts[k+1] <= infos_[i].start_moment_)
	k++;

      infos_[i].beat_start_ = beat_starts[k];
    }
  
  beamify (subdivide_beams);
}


void
Beaming_info_list::beamify (bool subdivide_beams)
{
  if (infos_.size () <= 1)
    return;
  
  Drul_array<Beaming_info_list> splits;

  bool at_boundary = false;
  int m = best_splitpoint_index (&at_boundary);

  splits[LEFT].infos_ = vector<Beaming_info> (infos_.begin (),
					      infos_.begin () + m);
  splits[RIGHT].infos_ = vector<Beaming_info> (infos_.begin () + m,
					       infos_.end ());

  Direction d = LEFT;

  do
    {
      splits[d].beamify (subdivide_beams);
    }
  while (flip (&d) != LEFT);

  int middle_beams = ((at_boundary && subdivide_beams)
		      ? 1
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
}


void
Beaming_info_list::add_stem (Moment m, int b)
{
  infos_.push_back (Beaming_info (m, b));
}

Beaming_info_list::Beaming_info_list ()
{
}

int
Beaming_info_list::beamlet_count (int i, Direction d) const
{
  return infos_.at (i).beam_count_drul_[d];
}
