/*   
  newkey-def.hh -- declare Newkey_def
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef NEWKEY_DEF_HH
#define NEWKEY_DEF_HH

#include "protected-scm.hh"
#include "musical-pitch.hh"

class Newkey_def
{

public:
  Protected_scm  pitch_alist_;

  Newkey_def();
  
  /// return number of flats in key
  int flats_i () const;

  /// return number of sharps in key
  int sharps_i () const;
  int accs_i (int) const;
  
  void transpose (Musical_pitch d);
};

#endif /* NEWKEY_DEF_HH */


