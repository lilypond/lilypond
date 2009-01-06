/*
  beaming-info.cc -- implement Beam_rhythmic_element, Beaming_pattern

  A Beaming_pattern object takes a set of stems at given moments and calculates
  the pattern of their beam. That is, it works out, for each stem, how many
  beams should be connected to the right and left sides of that stem. In
  calculating this, Beaming_pattern takes into account
   - the rhythmic position of the stems
   - the options that are defined in Beaming_options

  source file of the GNU LilyPond music typesetter

  (c) 1999--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "beaming-pattern.hh"
#include "context.hh"

/*
  Represents a stem belonging to a beam. Sometimes (for example, if the stem
  belongs to a rest and stemlets aren't used) the stem will be invisible.

  The rhythmic_importance_ of an element tells us the significance of the
  moment at which this element occurs. For example, an element that occurs at
  a beat is more significant than one that doesn't. Smaller number are
  more important. The rhythmic_importance_ is decided and filled in by
  Beaming_pattern. A rhythmic_importance_ smaller than zero has extra
  significance: it represents the start of a beat and therefore beams may
  need to be subdivided.
*/
Beam_rhythmic_element::Beam_rhythmic_element ()
{
  start_moment_ = 0;
  rhythmic_importance_ = 0;
  beam_count_drul_[LEFT] = 0;
  beam_count_drul_[RIGHT] = 0;
  invisible_ = false;

}

Beam_rhythmic_element::Beam_rhythmic_element (Moment m, int i, bool inv)
{
  start_moment_ = m;
  rhythmic_importance_ = 0;
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
Beam_rhythmic_element::count (Direction d) const
{
  return beam_count_drul_[d];
}

/*
  Finds the appropriate direction for the flags at the given index that
  hang below the neighbouring flags. If
  the stem has no more flags than either of its neighbours, this returns
  CENTER.
*/
Direction
Beaming_pattern::flag_direction (Beaming_options const &options, vsize i) const
{
  // The extremal stems shouldn't be messed with, so it's appropriate to
  // return CENTER here also.
  if (i == 0 || i == infos_.size () - 1)
    return CENTER;

  int count = infos_[i].count (LEFT); // Both directions should still be the same
  int left_count = infos_[i-1].count (RIGHT);
  int right_count = infos_[i+1].count (LEFT);

  // If we are told to subdivide beams and we are next to a beat, point the
  // beamlet away from the beat.
  if (options.subdivide_beams_)
    {
      if (infos_[i].rhythmic_importance_ < 0)
	return RIGHT;
      else if (infos_[i+1].rhythmic_importance_ < 0)
	return LEFT;
    }

  if (count <= left_count && count <= right_count)
    return CENTER;

  // Try to avoid sticking-out flags as much as possible by pointing my flags
  // at the neighbour with the most flags.
  else if (right_count > left_count)
    return RIGHT;
  else if (left_count > right_count)
    return LEFT;

  // If all else fails, point the beamlet away from the important moment.
  return (infos_[i].rhythmic_importance_ <= infos_[i+1].rhythmic_importance_) ? RIGHT : LEFT;
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

  find_rhythmic_importance (options);

  for (vsize i = 1; i < infos_.size () - 1; i++)
    {
      Direction non_flag_dir = other_dir (flag_direction (options, i));
      if (non_flag_dir)
	{
	  int importance = (non_flag_dir == LEFT)
	    ? infos_[i].rhythmic_importance_ : infos_[i+1].rhythmic_importance_;
	  int count = (importance < 0 && options.subdivide_beams_)
	    ? 1 : min (infos_[i].count (non_flag_dir),
		       infos_[i+non_flag_dir].count (-non_flag_dir));

	  infos_[i].beam_count_drul_[non_flag_dir] = count;
	}
    }
}

void
Beaming_pattern::find_rhythmic_importance (Beaming_options const &options)
{
  Moment measure_pos (0);  
  SCM grouping = options.grouping_;
  vsize i = 0;

  // Mark the importance of stems that start at a beat or a beat group.
  while (i < infos_.size ())
    {
      // If a beat grouping is not specified, default to 2 beats per group.
      int count = 2;
      if (scm_is_pair (grouping))
	{
	  count = scm_to_int (scm_car (grouping));
	  grouping = scm_cdr (grouping);
	}

      // Mark the start of this beat group
      if (infos_[i].start_moment_ == measure_pos)
	infos_[i].rhythmic_importance_ = -2;

      // Mark the start of each beat up to the end of this beat group.
      for (int beat = 1; beat <= count; beat++)
	{
	  Moment next_measure_pos = measure_pos + options.beat_length_;

	  while (i < infos_.size () && infos_[i].start_moment_ < next_measure_pos)
	    {
	      Moment dt = infos_[i].start_moment_ - measure_pos;

	      // The rhythmic importance of a stem between beats depends on its fraction
	      // of a beat: those stems with a lower denominator are deemed more
	      // important.
	      // FIXME: This is not the right way to do things for tuplets. For example,
	      // in an 8th-note triplet with a quarter-note beat, 1/3 of a beat should be
	      // more important than 1/2.
	      if (infos_[i].rhythmic_importance_ >= 0)
		infos_[i].rhythmic_importance_ = (dt / options.beat_length_).den ();

	      i++;
	    }

	  measure_pos = next_measure_pos;
	  if (i < infos_.size () && infos_[i].start_moment_ == measure_pos)
	    infos_[i].rhythmic_importance_ = -1;
	}
    }
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
	int b = min (infos_[i].count (LEFT), infos_[i-1].count (LEFT));
	infos_[i].beam_count_drul_[LEFT] = b;
	infos_[i].beam_count_drul_[RIGHT] = b;
      }

  for (vsize i = infos_.size (); i--;)
    if (infos_[i].invisible_)
      {
	int b = min (infos_[i].count (LEFT), infos_[i+1].count (LEFT));
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
