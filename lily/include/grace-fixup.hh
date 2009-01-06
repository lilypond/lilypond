/*
  grace-fixup.hh -- declare Grace_fixup

  source file of the GNU LilyPond music typesetter

  (c) 2004--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef GRACE_FIXUP_HH
#define GRACE_FIXUP_HH

#include "moment.hh"

/*
  This is a lookahead list for grace notes.

  {  ... X \grace Y  Z ... }

  normally, the ending of X is the start of Z. In case of a grace
  note, we take off a little at the end of X. What is stored: START
  (start point of X), LENGTH (length of X), GRACE_START (start_music
  of Y), and the next fixup element.

  This is also done for nested musics, i.e.

  voiceA = \notes { \grace b16 c'2 }
  voiceB = \notes { c'2 \voiceA }

  the iterator for voiceB will contain a fixup entry with (START = 0/1,
  LENGTH =2/1, GRACE_START =(0G-1/16) )

  Graces at the start of a sequential music iterator are handled
  by initting here_mom_ with Music::start_music (); no fixups are needed.
*/
struct Grace_fixup
{
  Moment start_;
  Moment length_;

  Rational grace_start_;

  Grace_fixup *next_;
};

#endif /* GRACE_FIXUP_HH */

