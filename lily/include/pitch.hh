/*   
  pitch.hh -- declare Pitch
  
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
class Pitch
{
public:				// fixme
  /*
    TODO: use SCM -- (make private?)
   */

    /// 0 is c, 6 is b
  int notename_i_;
  
  /// 0 natural, 1 sharp, etc
  int alteration_i_;

  /// 0 is central c
  int octave_i_;
  /*
    mutators, so JUNKME.
   */
  void transpose (Pitch);
  void up_to (int);
  void down_to (int);

public:

  int octave_i () const;
  int notename_i () const;
  int alteration_i () const;

  /*
    Pitch is lexicographically ordered by (octave, notename,
    alteration).    
   */
  Pitch (int octave, int notename,int accidental);
  Pitch ();

  Pitch to_relative_octave (Pitch);

  static int compare (Pitch const&,Pitch const&);
  /// return large part of interval from central c
  int steps() const;
  /// return pitch from central c (in halfnotes)
  int semitone_pitch() const; 
  String str () const;

  static SCM transpose (SCM p, SCM delta);

  SCM smobbed_copy () const;
  DECLARE_SCHEME_CALLBACK(less_p, (SCM a, SCM b));
  DECLARE_SIMPLE_SMOBS(Pitch,);


};

Pitch* unsmob_pitch (SCM);

#include "compare.hh"
INSTANTIATE_COMPARE(Pitch, Pitch::compare);

int compare (Array<Pitch>*, Array<Pitch>*);

#endif /* MUSICAL_PITCH_HH */


