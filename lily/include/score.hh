/*
  score.hh -- declare Score

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SCORE_HH
#define SCORE_HH

#include "array.hh"
#include "lily-proto.hh"
#include "plist.hh"
#include "string.hh"
#include "input.hh"
#include "lily-proto.hh"
#include "parray.hh"

/// the total music def of one movement
class Score: public Input {
public:
  /// paper_, staffs_ and commands_ form the problem definition.
  Link_array<Music_output_def> def_p_arr_;
  Music * music_p_;
  Scope * header_p_;

  int errorlevel_i_;
    
  /// construction
  Score();
  Score (Score const&);
  ~Score();    

  void process();
  void add_output (Music_output_def *def_p);
  void print() const;
private:
  void run_translator (Music_output_def*);
};

#endif
