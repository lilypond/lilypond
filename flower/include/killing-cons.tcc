/*   
  killing-cons.tcc -- declare Killing_cons
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef KILLING_CONS_TCC
#define KILLING_CONS_TCC

#include "cons.hh"

template<class T>
Killing_cons<T>::~Killing_cons ()
{
  delete car_p_;
}

template<class T>
void 
copy_killing_cons_list (Cons_list<T> &dest, Cons<T> *src) 
{
  for (; src; src  = src->next_cons_p_)
    {
      T *t = new T(*src->car_p_);
      dest.append ( new Killing_cons<T> (t, 0));
    }
}

template<class T>
void
clone_killing_cons_list (Cons_list<T> & dest, Cons<T> *src)
{
  for (; src; src  = src->next_cons_p_)
    {
      T *t = src->car_p_->clone ();
      dest.append (new Killing_cons<T> (t, 0));      
    }
}

#endif /* KILLING_CONS_TCC */

