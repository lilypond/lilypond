/*
  beaming-info.cc -- implement Beam_rhythmic_element, Beaming_pattern

  A Beaming_pattern object takes a set of stems at given moments and calculates
  the pattern of their beam. That is, it works out, for each stem, how many
  beams should be connected to the right and left sides of that stem. In
  calculating this, Beaming_pattern takes into account
   - the rhythmic position of the stems
   - the options that are defined in Beaming_options

  source file of the GNU LilyPond music typesetter

  (c) 1999--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "beaming-pattern.hh"
#include "context.hh"

Beam_rhythmic_element::Beam_rhythmic_element ()
{
  start_moment_ = 0;
  beam_count_drul_[LEFT] = 0;
  beam_count_drul_[RIGHT] = 0;
  invisible_ = false;

}

Beam_rhythmic_element::Beam_rhythmic_element (Moment m, int i, bool inv)
{
  start_moment_ = m;
  beam_count_drul_[LEFT] = i;
  beam_count_drul_[RIGHT] = i;
  invisible_ = inv;
}


void
Beam_rhythmic_element::de_grace ()
{
  if (start_moment_.grace_part_)
    {
      start_moment_.main_part_ = start_moment_.grace_part_; 
      start_moment_.grace_part_ = 0;
    }
}

int
count_factor_twos (int x)
{
  int c = 0;
  while (x && x % 2 == 0)
    {
      x /= 2;
      c ++;
    }
	 
  return c;
}

bool
Beaming_pattern::is_next_to_invisible_stem (vsize i) const
{
  return infos_[i].invisible_
    || (i > 0 && infos_[i-1].invisible_);
}

/*
  Finds the best point at which to subdivide a beam. In order of priority
  (highest to lowest) this is
   - before or after an invisible beam
       - at the start of a beat group
       - at the start of a beat
       - whose position in its beat has the smallest denominator (that is,
         a stem that starts halfway through a beat will be preferred over stems
         that start 1/3 or 2/3s of the way through the beat).
   - any other beam
       - at the start of a beat group
       - at the start of a beat
       - whose position in its beat has the smallest denominator

   If the split point occurs at the start of a beat group or at the start of a
   beat, at_boundary will be set to true. Otherwise, it will be set to false.
*/
int
Beaming_pattern::best_splitpoint_index (bool *at_boundary) const
{
  bool require_invisible = false;
  for (vsize i = 0; i < infos_.size (); i++)
    require_invisible |= infos_[i].invisible_;

  *at_boundary = true;
  for (vsize i = 1; i < infos_.size (); i++)  
    {
      if (infos_[i].group_start_ == infos_[i].start_moment_
	  && (!require_invisible || is_next_to_invisible_stem (i)))
	return i;
    }

  for (vsize i = 1; i < infos_.size (); i++)  
    {
      if (infos_[i].beat_start_ == infos_[i].start_moment_
	  && (!require_invisible || is_next_to_invisible_stem (i)))
	return i;
    }

  *at_boundary = false;
  
  int min_den = INT_MAX;
  int min_index = -1;
  
  for (vsize i = 1; i < infos_.size (); i++)  
    {
      Moment dt = infos_[i].start_moment_ - infos_[i].beat_start_;

      /*
	This is a kludge, for the most common case of 16th, 32nds
	etc. What should really happen is that \times x/y should
	locally introduce a voice-specific beat duration.  (or
	perhaps: a list of beat durations for nested tuplets.)
	
       */
      
      dt /= infos_[i].beat_length_;
      
      if (dt.den () < min_den
	  && (!require_invisible || is_next_to_invisible_stem (i)))
	{
	  min_den = dt.den ();
	  min_index = i;
	}
    }

  return min_index;
}

int
Beaming_pattern::beam_extend_count (Direction d) const
{
  if (infos_.size () == 1)
    return infos_[0].beam_count_drul_[d];

  Beam_rhythmic_element thisbeam = boundary (infos_, d, 0);
  Beam_rhythmic_element next = boundary (infos_, d, 1);

  return min (thisbeam.beam_count_drul_[-d], next.beam_count_drul_[d]);
}

void
Beaming_pattern::de_grace ()
{
  for (vsize i = 0; i < infos_.size (); i ++)
    {
      infos_[i].de_grace ();
    }
}

void
Beaming_pattern::beamify (Beaming_options const &options)
{
  unbeam_invisible_stems ();

  if (infos_.size () <= 1)
    return;

  if (infos_[0].start_moment_.grace_part_)
    de_grace ();

  if (infos_[0].start_moment_ < Moment (0))
    for (vsize i = 0; i < infos_.size (); i++)
      infos_[i].start_moment_ += options.measure_length_;
  
  Moment measure_pos (0);
  
  vector<Moment> group_starts;
  vector<Moment> beat_starts;

  // Find the starting moments of the beats and the groups.
  SCM grouping = options.grouping_;
  while (measure_pos <= infos_.back ().start_moment_)
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
	  beat_starts.push_back (measure_pos + options.beat_length_ * i);
	}
      measure_pos += options.beat_length_ * count;
    }

  // Set the group_start_ and beam_start_ properties of each stem.
  vsize j = 0;
  vsize k = 0;
  for (vsize i = 0; i  < infos_.size (); i++)
    {
      while (j + 1 < group_starts.size ()
	     && group_starts[j+1] <= infos_[i].start_moment_)
	j++;

      if (j < group_starts.size ())
	infos_[i].group_start_ = group_starts[j];
      
      infos_[i].beat_length_ = options.beat_length_;  
      while (k + 1 < beat_starts.size () 
	     && beat_starts[k+1] <= infos_[i].start_moment_)
	k++;

      if (k < beat_starts.size ())
	infos_[i].beat_start_ = beat_starts[k];
    }
  
  beamify (options.subdivide_beams_);
}


/*
  Determines beaming patterns by splitting the list of stems recursively.

  Given a list of stems, we split it at the most 'natural' place (eg. at
  the beginning of a beat) as determined by best_splitpoint_index. We
  then beamify both subsets of beams. Finally, we determine the number of
  beams between the subsets.

  Note: if one of the subsets has only one element, we will not modify its
  beams. This ensure that, for every stem, we will modify its beam count on at
  most one side.
*/
void
Beaming_pattern::beamify (bool subdivide_beams)
{
  if (infos_.size () <= 1)
    return;
  
  Drul_array<Beaming_pattern> splits;

  bool at_boundary = false;
  int m = best_splitpoint_index (&at_boundary);

  splits[LEFT].infos_ = vector<Beam_rhythmic_element> (infos_.begin (),
						       infos_.begin () + m);
  splits[RIGHT].infos_ = vector<Beam_rhythmic_element> (infos_.begin () + m,
							infos_.end ());

  Direction d = LEFT;

  do
    {
      splits[d].beamify (subdivide_beams);
    }
  while (flip (&d) != LEFT);

  int middle_beams = (at_boundary && subdivide_beams)
		      ? 1	
		      : min (splits[RIGHT].beam_extend_count (LEFT),
			     splits[LEFT].beam_extend_count (RIGHT));

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


/*
  Invisible stems should be treated as though they have the same number of
  beams as their least-beamed neighbour. Here we go through the stems and
  modify the invisible stems to satisfy this requirement.
*/
void
Beaming_pattern::unbeam_invisible_stems ()
{
  for (vsize i = 1; i < infos_.size (); i++)
    if (infos_[i].invisible_)
      {
	int b = infos_[i-1].beam_count_drul_[LEFT];
	infos_[i].beam_count_drul_[LEFT] = b;
	infos_[i].beam_count_drul_[RIGHT] = b;
      }

  for (vsize i = infos_.size (); i--;)
    if (infos_[i].invisible_)
      {
	int b = min (infos_[i].beam_count_drul_[LEFT], infos_[i+1].beam_count_drul_[LEFT]);
	infos_[i].beam_count_drul_[LEFT] = b;
	infos_[i].beam_count_drul_[RIGHT] = b;
      }
}


void
Beaming_pattern::add_stem (Moment m, int b, bool invisible)
{
  infos_.push_back (Beam_rhythmic_element (m, b, invisible));
}

Beaming_pattern::Beaming_pattern ()
{
}

int
Beaming_pattern::beamlet_count (int i, Direction d) const
{
  return infos_.at (i).beam_count_drul_[d];
}

void
Beaming_options::from_context (Context *context)
{
  grouping_ = context->get_property ("beatGrouping");
  subdivide_beams_ = to_boolean (context->get_property ("subdivideBeams"));
  beat_length_ = robust_scm2moment (context->get_property ("beatLength"), Moment (1, 4));
  measure_length_ = robust_scm2moment (context->get_property ("measureLength"), Moment (1, 4));
}

Beaming_options::Beaming_options ()
{
  grouping_ = SCM_EOL;
  subdivide_beams_ = false;
}
