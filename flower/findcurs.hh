template<class T, class U>
class FindCursor : public Cursor<T>
{
public:
    FindCursor( List<T>& list, Link<T>* pointer = 0 );
    FindCursor( const Cursor<T>& cursor );
    
    Cursor<T> operator =( const Cursor<T>& c );
    FindCursor<T, U> find( const T& thing );
};

#include "findcurs.inl"
