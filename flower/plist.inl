/* -*-c++-*-
  plist.inl -- part of flowerlib

  (c) 1996 Han-Wen Nienhuys& Jan Nieuwenhuizen
*/

#ifndef PLIST_INL
#define PLIST_INL



template<class T>
inline
PointerList<T>::~PointerList()
{
    Cursor<T> next(*this);
    for ( Cursor<T> c( *this ); c.ok(); c = next ) {
	next = c;
	next++;
	remove( c );		// PointerList::remove deletes the  real data
    }
}

template<class T>
inline void
PointerList_print( PointerList<T> const & l  ) 
{
    for (PCursor<T> c(l ); c.ok(); c++ )
        c->print();  
}

template<class T>
inline void
PL_copy(PointerList<T*> &to,PointerList<T*> const&src)
{
    for (PCursor<T*> pc(src); pc.ok(); pc++) {
	T *q = pc;
	T *p=new T(*q) ; // argh, how do i do this in ANSI-C++
	to.bottom().add(p);
    }
}
#endif
