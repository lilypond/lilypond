/*
  ps-score.hh -- declare Ps_score

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <jannneke@gnu.org>
*/

#ifndef PS_SCORE_HH
#define PS_SCORE_HH

#include "p-score.hh"

class Ps_score : public Paper_score
{
public:    
  virtual Paper_outputter* paper_outputter_p (Paper_stream*) const;
  virtual Paper_stream* paper_stream_p () const;
};

#endif // PS_SCORE_HH
