#include "plist.hh"

template<class T>
IPointerList<T>::~IPointerList()
{
    PCursor<T> c( *this );
    while (c.ok()) {
	c.del();
    }
}

template<class T>
PCursor<T> 
PointerList<T>::find(T what ) const
{
    PCursor<T> i(*this);
    for (; i.ok(); i++)
	if (i.ptr() == what)
	   break;
    return i;    
}
