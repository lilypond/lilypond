#ifndef CURSOR_CC
#define CURSOR_CC

#include "cursor.hh"
#include <assert.h>

template<class T>
void
Cursor<T>::backspace()
{
  Cursor<T> c (*this);
  if (c.ok())
    c--;        
  list_l_->remove (*this);
}

template<class T>
void
Cursor<T>::del()
{
  Cursor<T> c (*this);
  if (c.ok())
    c++;
  list_l_->remove (*this);    
  *this = c;
}


template<class T>
Cursor<T> 
Cursor<T>::operator -=(int j)    
{
  while (j--)
    (*this)--;
  return *this;
}
template<class T>
Cursor<T> 
Cursor<T>::operator +=(int j)    
{
  while (j++)
    (*this)++;
  return *this;
}

template<class T>
Cursor<T> 
Cursor<T>::operator +(int i) const    
{
  Cursor<T> r = *this;

  if (i<0)
    return r -(-i);

  while (i--)
    r++;

  return r;
}

template<class T>
Cursor<T>
Cursor<T>::operator -(int i) const
{
  Cursor<T> r = *this;
  if (i<0)
    return r +(-i);

  while (i--)
    r--;
  
  return r;
}
/*
  warning:  can't use Cursor::operator == (Cursor),
  since it uses Cursor::operator-(Cursor)
 */
template<class T>
int
Cursor<T>::operator-(Cursor<T> rhs) const
{
  assert (rhs.list_l_ == list_l_);
  int dif = 0;

  // search from *this on further up (positive difference)
  Cursor<T> c (*this);
  while (c.ok() && c.pointer_ != rhs.pointer_) 
    {
      c--;
      dif++;
    }
  
  if (c.ok())
    goto gotcha;		// so, sue me.

  // search in direction of bottom. (negative diff)
  dif =0;
  c=*this;    
  while (c.ok() && c.pointer_ !=rhs.pointer_) 
    {
      dif --;
      c++;
    }
  assert (c.ok());

 gotcha:
  assert ((*this - dif).pointer_ == c.pointer_);
  return dif;
}

#endif
