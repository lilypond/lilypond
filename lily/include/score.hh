/*
  score.hh -- declare Score

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SCORE_HH
#define SCORE_HH

#include "input.hh"
#include "lily-proto.hh"
#include "protected-scm.hh"
#include "parray.hh"

/// the total music def of one movement
class Score: public Input {
public:
  /// paper_, staffs_ and commands_ form the problem definition.
  Link_array<Music_output_def> def_p_arr_;
  Protected_scm music_;
  Scope * header_p_;

  int errorlevel_i_;
    
  /// construction
  Score();
  Score (Score const&);
  ~Score();    

  void process();
  void add_output (Music_output_def *def_p);
private:
  void run_translator (Music_output_def*);
};

#endif
