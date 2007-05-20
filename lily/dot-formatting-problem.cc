
#include "dot-formatting-problem.hh"
#include "dot-configuration.hh"
#include "skyline.hh"

Dot_formatting_problem::~Dot_formatting_problem()
{
  delete best_;
}

void
Dot_formatting_problem::register_configuration (Dot_configuration const &src)
{
  int b = src.badness ();
  if (b < score_)
    {
      delete best_;
      best_ = new Dot_configuration (src);
    }
}

Dot_configuration *
Dot_formatting_problem::best () const
{
  return best_;
}



Dot_formatting_problem::Dot_formatting_problem (vector<Box> const &boxes, Interval base_x)
  : head_skyline_ (boxes, 0.0, Y_AXIS, RIGHT)
{
  best_ = 0;
  head_skyline_.set_minimum_height (base_x[RIGHT]);
  score_ = 1 << 30;
}
