/*
  score.hh -- declare Score

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SCORE_HH
#define SCORE_HH

#include "input.hh"
#include "lily-proto.hh"

#include "parray.hh"
#include "smobs.hh"

/// the total music def of one movement
class Score: public Input {
public:
  /// paper_, staves_ and commands_ form the problem definition.
  Link_array<Music_output_def> defs_;
  SCM music_;
  Scheme_hash_table * header_;

  int errorlevel_;
    
  /// construction
  Score ();
  Score (Score const&);


  void process ();
  void add_output (Music_output_def *def);
  DECLARE_SMOBS (Score,foo);
private:
  void run_translator (Music_output_def*);
};
DECLARE_UNSMOB(Score,score); 
#endif
