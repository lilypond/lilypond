/*   
     beaming-info.cc --  implement Beaming_info, Beaming_info_list
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "beaming.hh"

Beaming_info::Beaming_info ()
{
  start_mom_ = 0;
  beams_i_drul_[LEFT] = 0;
  beams_i_drul_[RIGHT] = 0;  
}

Beaming_info::Beaming_info (Moment m, int i)
{
  start_mom_ = m;
  beams_i_drul_[LEFT] = i;
  beams_i_drul_[RIGHT] = i;  
}

const int infinity_i = INT_MAX;	// guh.
const int at_beat = 1<<15;

int
Beaming_info_list::best_splitpoint_index (Moment &beat_length,bool subdivide) const
{
  int minden = infinity_i;
  int minidx = -1;
  Moment beat_pos;

  for (int i=1; i < infos_.size (); i++)
    {
      beat_pos = infos_[i].start_mom_ / beat_length;
      int den = beat_pos.den ();
      if (infos_[i].beams_i_drul_[LEFT] == infos_[i-1].beams_i_drul_[RIGHT] && !subdivide)
	den *= 2;
      if (den < minden)
	{
	  minidx = i;
	  minden = den;
	}
    }

  return minidx|(minden==1 && subdivide ? at_beat : 0);
}

int
Beaming_info_list::beam_extend_count (Direction d) const
{
  if (infos_.size () == 1)
    return infos_[0].beams_i_drul_[d];

  Beaming_info thisbeam  = infos_.boundary (d, 0);
  Beaming_info next  = infos_.boundary (d, 1);
  
  return thisbeam.beams_i_drul_[-d] <? next.beams_i_drul_[d];
}

void
Beaming_info_list::beamify (Moment &beat_length,bool subdivide)
{
  if (infos_.size () <= 1)
    return;
      
  Drul_array<Beaming_info_list> splits;
  int m = best_splitpoint_index (beat_length,subdivide);
  bool split = subdivide && (m & at_beat);  m = m & ~at_beat;
  splits[LEFT].infos_ = infos_.slice (0,m);
  splits[RIGHT].infos_ = infos_.slice (m, infos_.size ());

  Direction d = LEFT;
 
  do
    {
      splits[d].beamify (beat_length,subdivide);
    }
  while (flip (&d) != LEFT);

  int middle_beams = (split ? 1 :
		      splits[RIGHT].beam_extend_count (LEFT) <?
		      splits[LEFT].beam_extend_count (RIGHT));

  do
    {
      if (splits[d].infos_.size () != 1)
	{
	  splits[d].infos_.boundary (-d, 0).beams_i_drul_[-d] = middle_beams;
	}
    }
  while (flip (&d) != LEFT);

  infos_ = splits[LEFT].infos_;
  infos_.concat (splits[RIGHT].infos_);
}

void
Beaming_info_list::add_stem (Moment m, int b)
{
  infos_.push (Beaming_info (m, b));
}
