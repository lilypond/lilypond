// cursor.cc
#ifndef CURSOR_CC
#define CURSOR_CC

#include "cursor.hh"
//#define inline
//#include "cursor.inl"
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

#endif
