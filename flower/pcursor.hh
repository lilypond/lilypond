

/// cursor which feels like a pointer
template<class T>
struct PCursor : public Cursor<T> {

    /// make cursor with #no# items back
    PCursor<T> operator -( int no) const {
	return PCursor<T> (Cursor<T>::operator-(no));
    }

    /// make cursor with #no# items further
    PCursor<T> operator +( int no) const {
	return PCursor<T> (Cursor<T>::operator+(no));
    }
    PCursor(List<T> & l) : Cursor<T> (l) {}

    PCursor( const Cursor<T>& cursor ) : Cursor<T>(cursor) { }
    T operator ->() { return  *(*this); }

};
/**
 HWN: I'd like an operator->(), so here it is.

 Cursor to go with pointer list.
 */
