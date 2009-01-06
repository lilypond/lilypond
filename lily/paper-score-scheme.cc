/*
  paper-score-scheme.cc --  implement Paper_score bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "paper-score.hh"

LY_DEFINE (ly_paper_score_paper_systems, "ly:paper-score-paper-systems",
	   1, 0, 0,
	   (SCM paper_score),
	   "Return vector of @code{paper_system} objects from"
	   " @var{paper-score}.")
{
  LY_ASSERT_TYPE (unsmob_paper_score, paper_score, 1);

  Paper_score *pscore = dynamic_cast<Paper_score *> (unsmob_music_output (paper_score));
  return pscore->get_paper_systems ();
}
