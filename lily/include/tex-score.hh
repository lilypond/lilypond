/*
  tex-score.hh -- declare Tex_score

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <jannneke@gnu.org>
*/

#ifndef TEX_SCORE_HH
#define TEX_SCORE_HH

#include "p-score.hh"

class Tex_score : public Paper_score
{
public:    
  virtual Paper_outputter* paper_outputter_p (Paper_stream*) const;
  virtual Paper_stream* paper_stream_p () const;
};

#endif // TEX_SCORE_HH
