
/*
  pcursor.hh -- part of flowerlib

  (c) 1996 Han-Wen Nienhuys&Jan Nieuwenhuizen
*/

#ifndef PCURSOR_HH
#define PCURSOR_HH


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
    PCursor(const List<T> & l) : Cursor<T> (l) {}

    PCursor( const Cursor<T>& cursor ) : Cursor<T>(cursor) { }
    T operator ->() const { return  *(*(Cursor<T> *)this); }

};
/**
 I like  operator->(), so here it is.

 Cursor to go with pointer list.
 */
#endif
