 // cursor.inl -*-c++-*-
#ifndef CURSOR_INL
#define CURSOR_INL
#include <assert.h>


template<class T>
inline
Cursor<T>::Cursor( const List<T>& list, Link<T>* pointer ) : 
    list_((List<T>&) list )
{
    if ( list.size() )
        pointer_ = pointer ? pointer : list.top_;
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
Cursor<T>::thing()
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
Cursor<T>::add( const T& th )
{
    list_.add( th, *this );
}

template<class T>
inline void
Cursor<T>::insert( const T& th )
{
    list_.insert( th, *this );
}

template<class T>
inline  List<T>&
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


template<class T>
inline Cursor<T> 
Cursor<T>::operator ++( int )    
{
    Cursor<T> r (*this);
    assert( pointer_ );
    pointer_ = pointer_->next();
    return r;
}

template<class T>
inline Cursor<T>
Cursor<T>::operator --( int )
{
    Cursor<T> r (*this);
    assert( pointer_ );
    pointer_ = pointer_->previous();
    return r;
}

#endif
