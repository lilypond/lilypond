/*   
  musical-pitch.hh -- declare Musical_pitch
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef MUSICAL_PITCH_HH
#define MUSICAL_PITCH_HH

#include "lily-proto.hh"
#include "input.hh"

/** A "tonal" pitch. This is a pitch as it figures in diatonal western
   music (12 semitones in an octave), as opposed to a frequence in Hz
   or a integer number of semitones.

   It is not Music because, it has no duration associated.
*/
struct Musical_pitch : public Input
{
  Musical_pitch (int notename=0, int accidental=0, int octave=0, bool cautionary=false);

  /// 0 is c, 6 is b
  int notename_i_;
  /// 0 natural, 1 sharp, etc
  int accidental_i_;
  /// 0 is central c
  int octave_i_;
  /// Used for cautionary accidentals
  bool cautionary_b_;

  Musical_pitch to_relative_octave (Musical_pitch);
  void transpose (Musical_pitch);
  static int compare (Musical_pitch const&,Musical_pitch const&);
  /// return large part of interval from central c
  int steps() const;
  /// return pitch from central c (in halfnotes)
  int semitone_pitch() const; 
  void up_to (int);
  void down_to (int);
  String str () const;
  void print () const;
};

#include "compare.hh"
INSTANTIATE_COMPARE(Musical_pitch, Musical_pitch::compare);

#endif /* MUSICAL_PITCH_HH */

