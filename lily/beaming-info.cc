/*   
     beaming-info.cc --  implement Beaming_info, Beaming_info_list
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "beaming.hh"

Beaming_info::Beaming_info( )
{
  start_mom_ = 0;
  beams_i_drul_[LEFT] = 0;
  beams_i_drul_[RIGHT] = 0;  
}

Beaming_info::Beaming_info(Moment m, int i)
{
  start_mom_ = m;
  beams_i_drul_[LEFT] = i;
  beams_i_drul_[RIGHT] = i;  
}

const int infinity_i = INT_MAX;	// guh.

int
Beaming_info_list::min_denominator_index () const
{
  int minden = infinity_i;
  int minidx = -1;

  for (int i=1; i < infos_.size ( ); i++)
    {
      if (infos_[i].start_mom_.den_i () < minden)
	{
	  minidx = i;
	  minden = infos_[i].start_mom_.den_i  ();
	}
    }

  return minidx;
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
Beaming_info_list::beamify ()
{
  if (infos_.size () <= 1)
    return;
      
  Drul_array<Beaming_info_list> splits;
  int m = min_denominator_index ();
  splits[LEFT].infos_ = infos_.slice (0,m);
  splits[RIGHT].infos_ = infos_.slice (m, infos_.size ());

  Direction d = LEFT;
 
  do
    {
      splits[d].beamify ();
    }
  while (flip (&d) != LEFT);

  int middle_beams = splits[RIGHT].beam_extend_count (LEFT) <?
    splits[LEFT].beam_extend_count (RIGHT);

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
  infos_.push  (Beaming_info (m, b));
}
