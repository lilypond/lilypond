/*
  pointer.tcc -- implement P

  source file of the Flower Library

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef POINTER_TCC
#define POINTER_TCC

template<class T>
inline
T *
P<T>::copy_p() const
{
  return t_p? new T(*t_p) : 0;
}

template<class T>
inline
void
P<T>::copy (T const *l_C)
{
  t_p = l_C ? new T(*l_C) : 0;
}

template<class T>
inline
void
P<T>::junk()
{
  delete t_p;
  t_p =0;
}

template<class T>
inline
P<T>::P(P<T> const &s) 
{
  t_p = s.copy_p();
}

template<class T>
inline
P<T> &
P<T>::operator =(P const&s)
{
  junk();
  copy (s.t_p);
  return *this;
}

template<class T>
inline
P<T>::~P() {
  junk();
}

template<class T>
inline
void
P<T>::set_p (T * np) 
{
  if (np == t_p)
    return;
  delete t_p;
  
  t_p = np;
}


template<class T>
inline
void
P<T>::set_l (T const * l_C) 
{
  if (t_p == l_C)
    return;
  
  junk();
  copy (l_C);
}



#endif // POINTER_TCC
