/*   
  repeated-music.hh -- declare Repeated_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef RepEATED_MUSIC_HH
#define RepEATED_MUSIC_HH

#include "music.hh"


/**
   Musical repeats.

   A repeat, when played has the form of BODY A BODY B BODY C.

   In this examples, the list {A B C} is put in ALTERNATIVES_P_.  BODY
   is put in REPEAT_BODY_P_.  Either the body or the alternative may
   be omitted.

   There are three modes of representing this  music:

   BODY A
        B
	C

   is called "folded". Mostly used for lyrics.

   
   BODY A B C

   is called volta.  This is common notation

   BODY A BODY B BODY C

   is called unfolded.  Useful for MIDI.

   If the number of repeats is smaller than the number of alternatives, then
   the excess alternatives are ignored for all timing purposes.

   If the number of repeats is bigger than the number of alternatives, then
   the first alternative is assumed to be repeated.
   
*/
class Repeated_music : public Music
{
public:
  Music * body () const;
  SCM alternatives () const;

  /// how often do we repeat?
  int repeat_count ( ) const;
  virtual Pitch to_relative_octave (Pitch);

  Moment body_length_mom () const;
  Moment alternatives_length_mom (bool fold) const;
  Moment alternatives_volta_length_mom () const;  

  DECLARE_SCHEME_CALLBACK (unfolded_music_length, (SCM));
  DECLARE_SCHEME_CALLBACK (volta_music_length, (SCM));
  DECLARE_SCHEME_CALLBACK (folded_music_length, (SCM));    
  
  /// Transpose, with the interval central C to #p#
  virtual void transpose (Pitch p);

  /// Scale the music in time by #factor#.
  virtual void compress (Moment factor);
  VIRTUAL_COPY_CONS (Music);
  Repeated_music ();
  Repeated_music (SCM);
};


#endif /* RepEATED_MUSIC_HH */
