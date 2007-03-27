#ifndef DOT_FORMATTING_PROBLEM_HH
#define DOT_FORMATTING_PROBLEM_HH


#include "skyline.hh"
#include "std-vector.hh"

#include <map>

struct Dot_formatting_problem
{
  Skyline head_skyline_;
  Dot_configuration *best_;
  int score_;

  void register_configuration (Dot_configuration const &);
  Dot_configuration *best () const;
  Dot_formatting_problem (vector<Box> const &boxes, Interval base_x);
  ~Dot_formatting_problem();
};

#endif
