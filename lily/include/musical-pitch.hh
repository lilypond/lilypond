/*   
  musical-pitch.hh -- declare Musical_pitch
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef MUSICAL_PITCH_HH
#define MUSICAL_PITCH_HH

#include "lily-proto.hh"
#include "input.hh"
#include "lily-guile.hh" // we need SCM

/** A "tonal" pitch. This is a pitch as it figures in diatonal western
   music (12 semitones in an octave), as opposed to a frequence in Hz
   or a integer number of semitones.

   It is not Music because, it has no duration associated.
*/
struct Musical_pitch : public Input
{
  Musical_pitch (int notename=0, int accidental=0, int octave=0);

  /// 0 is c, 6 is b
  int notename_i_;
  /// 0 natural, 1 sharp, etc
  int accidental_i_;
  /// 0 is central c
  int octave_i_;

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

SCM to_scm (Musical_pitch p);
void scm_to (SCM s, Musical_pitch* p);

#include "compare.hh"
INSTANTIATE_COMPARE(Musical_pitch, Musical_pitch::compare);

#endif /* MUSICAL_PITCH_HH */

