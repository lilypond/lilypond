/*   
  protected-scm.hh -- declare Protected_scm
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef PROTECTED_SCM_HH
#define PROTECTED_SCM_HH

#include <guile/gh.h>

/*
  Mix GUILE GC with C++ ctors and dtors. 
 */
class Protected_scm
{
  SCM object_;
public:
  Protected_scm ();
  Protected_scm (SCM);
  Protected_scm (Protected_scm const &);
  ~Protected_scm ();
  Protected_scm &operator = (Protected_scm const &);
  operator SCM () const;
  SCM to_SCM () const;
};

#endif /* PROTECTED_SCM_HH */
