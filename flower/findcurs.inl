template<class T, class U>
inline
FindCursor<T, U>::FindCursor( List<T>& list, Link<T>* pointer ) : 
    Cursor<T>( list, pointer )
{
}

template<class T, class U>
inline
FindCursor<T, U>::FindCursor( const Cursor<T>& cursor ) :
    Cursor<T>( cursor )
{
}

template<class T, class U>
Cursor<T>
FindCursor<T, U>::operator =( const Cursor<T>& c )
{   
    ( (Cursor<T>&)*this ) = c;
    return *this;
}

template<class T, class U>
inline FindCursor<T, U>
FindCursor<T, U>::find( const T& thing )
{
    Cursor<T> c( *this );
    while( c.forward() )
	{
	T& look = *c;
	if ( U::equal( look, thing ) )
	    return c;
	c++;
	}

    return FindCursor( *this ); // cursor is not .ok()
}
