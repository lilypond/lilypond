/*
  music-output.hh -- declare Music_output

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef MUSIC_OUTPUT_HH
#define MUSIC_OUTPUT_HH

#include "std-string.hh"
#include "lily-proto.hh"
#include "protected-scm.hh"
#include "smobs.hh"
#include "virtual-methods.hh"

class Music_output
{
  DECLARE_SMOBS (Music_output);
  DECLARE_CLASSNAME(Music_output);
protected:
  Music_output ();

public:
  virtual void derived_mark () const;
  virtual void process ();
};

DECLARE_UNSMOB (Music_output, music_output);
Paper_score *unsmob_paper_score (SCM);
Performance *unsmob_performance (SCM);
#endif /* MUSIC_OUTPUT_HH */
