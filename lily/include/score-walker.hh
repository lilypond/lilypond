/*
  score-walker.hh -- declare Score_walker

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>

    
*/


#ifndef SCOREWALKER_HH
#define SCOREWALKER_HH
#include "pcursor.hh"
#include "proto.hh"
#include "varray.hh"


/**
  walk through the score_columns, and while doing so, walk all staffs in a score.

  TODO
  support for vertical spanners.
  */
class Score_walker : public PCursor<Score_column *>
{
    Score* score_l_;
    /// walkers for the individual staves.
    Array<Staff_walker *> walker_p_arr_;
    Array<Staff_walker *> disallow_break_walk_l_arr;
    int disallow_break_count_;
    int breaks_i_;
    void reinit();
public:
    bool break_allowed_b();
    void allow_break(Staff_walker*w);
    Score_walker(Score*);
    ~Score_walker();
    Moment when();
    void operator++(int);
    /// process staff walkers. 
    void process();
};
#endif // SCOREWALKER_HH
