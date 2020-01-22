#ifndef DOT_FORMATTING_PROBLEM_HH
#define DOT_FORMATTING_PROBLEM_HH

#include "skyline.hh"
#include "std-vector.hh"

class Dot_formatting_problem
{
private:
  Skyline head_skyline_;

public:
  Skyline const &head_skyline () const { return head_skyline_; }

  Dot_formatting_problem (std::vector<Box> const &boxes, Interval base_x);
};

#endif
