#ifndef CURSOR_CC
#define CURSOR_CC

#include "cursor.hh"
#include <assert.h>

template<class T>
Cursor<T> 
Cursor<T>::operator ++( int )    
{
    Cursor<T> r = *this;
    assert( pointer_ );
    pointer_ = pointer_->next();
    return r;
}
template<class T>
Cursor<T> 
Cursor<T>::operator -=( int j )    
{
    while (j--)
	(*this)--;
    return *this;
}
template<class T>
Cursor<T> 
Cursor<T>::operator +=( int j )    
{
    while (j++)
	(*this)++;
    return *this;
}

template<class T>
Cursor<T>
Cursor<T>::operator --( int )
{
    Cursor<T> r = *this;
    assert( pointer_ );
    pointer_ = pointer_->previous();
    return r;
}

template<class T>
Cursor<T> 
Cursor<T>::operator +( int i ) const    
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
Cursor<T>::operator -( int i ) const
{
    Cursor<T> r = *this;
    if (i<0)
	return r +(-i);

    while (i--)
	r--;
    
    return r;
}

template<class T>
int
Cursor<T>::operator-(Cursor<T> c) const
{
    assert(c.list == list);
    int dif = 0;
    Cursor<T> upward(c);
    while (upward.ok() && upward.pointer_ != pointer_) {
	upward++;
	dif++;
    }
    
    if (upward.ok())
	return dif;
    dif =0;
    while (c.ok()&& c.pointer_ != pointer_) {
	dif --;
	c--;
    }
    assert(c.ok());
    return dif;
}

#endif
