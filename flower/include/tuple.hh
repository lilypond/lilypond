/*   
  tuple.hh -- declare Tuple
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef TUPLET_HH
#define TUPLET_HH
#error


template<class T, class U>
struct Tuple
{
  T e1_;
  U e2_;

  Tuple (T t, U u)
    {
      e1_ = t;
      e2_ = u;
    }
  Tuple ()
    {
    }
};



#endif /* TUPLET_HH */

