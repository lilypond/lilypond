
/*
  pcursor.hh -- part of flowerlib

  (c) 1996 Han-Wen Nienhuys&Jan Nieuwenhuizen
*/

#ifndef PCURSOR_HH
#define PCURSOR_HH


/// cursor to go with PointerList
template<class T>
struct PCursor : public Cursor<void *> {

    /// make cursor with #no# items back
    PCursor<T> operator -( int no) const {
	return PCursor<T> (Cursor<void*>::operator-(no));
    }
    int operator -(PCursor<T> op) const {
	return Cursor<void*>::operator-(op);
    }
    /// make cursor with #no# items further
    PCursor<T> operator +( int no) const {
	return PCursor<T> (Cursor<void*>::operator+(no));
    }
    
    PCursor(const PointerList<T> & l) : Cursor<void*> (l) {}

    PCursor( const Cursor<void*>& cursor ) : Cursor<void*>(cursor) { }
    void* vptr() const { return  * ((Cursor<void*> &) *this); }

    // should return T& ?
    T ptr() const { return (T) vptr(); }
    T operator ->() const { return  ptr(); }
    operator T() { return ptr(); }
    T operator *() { return ptr(); }
    void add(const T& p ) { Cursor<void*>::add((void*) p); }
    void insert(const T& p ) { Cursor<void*>::insert((void*) p);}

private:
//    Cursor<void*>::operator void*;
    // sigh
};
/**
  don't create PointerList<void*>'s.
  This cursor is just an interface class for Cursor. It takes care of the
  appropriate type casts
 */


template<class T>
inline  int pcursor_compare(PCursor<T> a,PCursor<T>b)
{
    return cursor_compare(Cursor<void*>(a),Cursor<void*> (b));
}

#include "compare.hh"
template_instantiate_compare(PCursor<T>, pcursor_compare, template<class T>);

#endif
