/*   
  newkey.hh -- declare Newkey
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef NEWKEY_HH
#define NEWKEY_HH

#include "protected-scm.hh" 

class Newkey
{
  /*
    alist mapping 
    (octave . notename) -> accidental and notename -> accidental
   */
  Protected_scm key_alist_;
public:
  void set (int name, int acc);
  void set (int oct, int name, int acc);
  void clear ();
  void set_scm (SCM k, SCM v);
  Newkey();
  int get (int oct, int name);
  int get (int name);
};
#endif /* NEWKEY_HH */

