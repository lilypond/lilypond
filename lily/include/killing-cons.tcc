/*   
  killing-cons.tcc -- declare Killing_cons
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef KILLING_CONS_TCC
#define KILLING_CONS_TCC


template<class T>
Killing_cons<T>::~Killing_cons ()
{
  delete car_;
}

template<class T>
Cons_list<T>
copy_killing_cons_list (Cons<T> *src)
{
  Cons_list<T> kl;

  for (; src; src  = src->next_)
    {
      T *t = new T(*src->car_);
      *kl.tail_ = new Killing_cons<T> (t, 0);
      kl.tail_ = &(*kl.tail_)->next_;
    }
  
  return kl;
}

template<class T>
Cons_list<T>
clone_killing_cons_list (Cons<T> *src)
{
  Cons_list<T> kl;

  for (; src; src  = src->next_)
    {
      T *t = src->car_->clone ();
      *kl.tail_ = new Killing_cons<T> (t, 0);
      kl.tail_ = &(*kl.pp)->next_;
    }
  
  return kl;
}


#endif /* KILLING_CONS_TCC */

