/*   
  killing-cons.tcc -- declare Killing_cons
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef KILLING_CONS_TCC
#define KILLING_CONS_TCC

template<class T>
Cons<T>::~Cons ()
{
}

template<class T>
Killing_cons<T>::~Killing_cons ()
{
  delete car_;
  delete next_;
}


#endif /* KILLING_CONS_TCC */

