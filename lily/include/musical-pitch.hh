/*   
  musical-pitch.hh -- declare Musical_pitch
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef MUSICAL_PITCH_HH
#define MUSICAL_PITCH_HH

#include "lily-proto.hh"
#include "smobs.hh"

/** A "tonal" pitch. This is a pitch as it figures in diatonal western
   music (12 semitones in an octave), as opposed to a frequence in Hz
   or a integer number of semitones.

   

   
*/
class Musical_pitch
{
public:				// fixme
  
  /// 0 is c, 6 is b
  int notename_i_;
  
  /// 0 natural, 1 sharp, etc
  int alteration_i_;

  /// 0 is central c
  int octave_i_;
public:

  int octave_i () const;
  int notename_i () const;
  int alteration_i () const;

  /*
    Musical_pitch is lexicographically ordered by (octave, notename,
    alteration).    
   */
  Musical_pitch (int octave, int notename,int accidental);
  Musical_pitch ();

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

  SCM smobbed_copy () const;
  DECLARE_SCHEME_CALLBACK(less_p, (SCM a, SCM b));
  DECLARE_SIMPLE_SMOBS(Musical_pitch,);
};

Musical_pitch* unsmob_pitch (SCM);

#include "compare.hh"
INSTANTIATE_COMPARE(Musical_pitch, Musical_pitch::compare);

int compare (Array<Musical_pitch>*, Array<Musical_pitch>*);

#endif /* MUSICAL_PITCH_HH */

