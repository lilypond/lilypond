#include "simple-spacer.hh"
#include "idealspacing.hh"
// #include ""


void
Spring_info::set(Idealspacing *i)
{
  space_f_ = i->space_f_;
  hooke_f_ = i->hooke_f_;
}

Spring_info::Spring_info()
{
  space_f_ = 0.0;
  hooke_f_ = 1.0;
  blocking_stretch_f_ = infinity_f;
  blocking_rod_l_ = 0;
}

  

void
Simple_spring_spacer::init ()
{
  for (PCursor<Rod_info*> i(rods_.top()); i.ok (); i++)
    {
      Real hooke=0.0;
      Real dist= i->distance_f_;

      hooke =0.0;
      
      for (int j = i->cols_[LEFT]; j < i->cols_[RIGHT]; j++)
	{
	  hooke += 1/springs_[j].hooke_f_;
	  dist -= springs_[j].space_f_;
	}
      
      hooke = 1/hooke;

      for (int j = i->cols_[LEFT]; j < i->cols_[RIGHT]; j++)
	{
	  Real block_stretch = hooke * dist / (springs_[j].space_f_ *
					       springs_[j].hooke_f_ );

	  if (block_stretch < springs_[j].blocking_stretch_f_)
	    {
	      springs_[j].blocking_stretch_f_ = block_stretch;
	      springs_[j].blocking_rod_l_ = i.ptr ();
	    }
	}
    }
}

Array<Real>
Simple_spring_spacer::solve ()
{
  Real start_force = 0.0;

  for (int i=0; i< springs_.size (); i++)
    {
      start_force = start_force >?
	springs_[i].blocking_stretch_f_ * springs_[i].hooke_f_;
    }

  Array<Real> stretch_factors;
  Array<bool> blocked;
  int blocked_count=0; 
  Real current_len =0.0;
  for (int i=0; i < springs_.size (); i++)
    {
      Real stretch = start_force / (springs_[i].hooke_f_ * springs_[i].space_f_);
      stretch_factors.push (stretch);
      current_len += (1  + stretch) * springs_[i].space_f_;
      blocked.push(false);
    }

 
  while (blocked_count < blocked.size ())
    {
      Real      next_stretch = -1.0;
      int block_index;
      for (int i=0; i< stretch_factors.size (); i++)
	{
	  if (!blocked[i])
	    {
	      next_stretch = next_stretch >? springs_[i].blocking_stretch_f_;

	      if (next_stretch < springs_[i].blocking_stretch_f_)
		{
		  next_stretch = springs_[i].blocking_stretch_f_;
		  block_index = i;
		}
	    }
	}
      current_len = 0.0;

      Real force = next_stretch * (springs_[block_index].space_f_* springs_[block_index].hooke_f_);
      for (int i=0; i< stretch_factors.size (); i++)
	{
	  if (!blocked[i])
	    {
	      stretch_factors[i] = force / (springs_[i].space_f_ * springs_[i].hooke_f_);
	    }
	  current_len += (1.0 + stretch_factors[i]) * springs_[i].space_f_;
	}

      Rod_info *blockrod = springs_[block_index].blocking_rod_l_;
      for (int j = blockrod->cols_ [LEFT]; j < blockrod->cols_ [RIGHT]; j++)
	{
	  blocked[j] = true;
	  blocked_count ++;
	}
    }


  Array<Real> distances;
  for (int i=0; i < stretch_factors.size (); i++)
    {
      distances.push (stretch_factors[i] * springs_[i].space_f_);
    }
  return distances;
}

