 // cursor.inl
#ifndef CURSOR_INL
#define CURSOR_INL
#include <assert.h>
//#include "list.hh"

template<class T>
inline
Cursor<T>::Cursor( List<T>& list, Link<T>* pointer ) : 
    list_( list )
{
    if ( list.size() )
        pointer_ = pointer ? pointer : list.top().pointer_;
    else
        pointer_ = pointer;
}

template<class T>
inline
Cursor<T>::Cursor( const Cursor<T>& cursor ) :
    list_( cursor.list_ )
{
    pointer_ = cursor.pointer_;
}

template<class T>
inline T&
Cursor<T>::operator *()
{
    assert( pointer_ );
    return pointer_->thing();
}

template<class T>
Cursor<T>
Cursor<T>::operator =( const Cursor<T>& c )
{   
    assert( &list_ == &c.list_ );
    pointer_ = c.pointer_;
    return *this;
}

template<class T>
inline void
Cursor<T>::add( const T& thing )
{
    list_.add( thing, *this );
}

template<class T>
inline void
Cursor<T>::insert( const T& thing )
{
    list_.insert( thing, *this );
}

template<class T>
inline void
Cursor<T>::remove()
{
    assert( pointer_ );
    list_.remove( *this );
}

template<class T>
inline const List<T>&
Cursor<T>::list() const
{
    return list_;
}

template<class T>
inline Link<T>*
Cursor<T>::pointer()
{
    return pointer_;
}

template<class T>
inline bool
Cursor<T>::backward()
{
    return ( pointer_ != 0 );
}

template<class T>
inline bool
Cursor<T>::forward()
{
    return ( pointer_ != 0 );
}

template<class T>
inline bool
Cursor<T>::ok()
{
    return ( pointer_ != 0 );
}

#endif