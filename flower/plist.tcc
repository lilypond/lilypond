#include "plist.hh"

// not inlined since it assumes knowledge of destructor.
template<class T>
void
IPointerList<T>::remove(PCursor<T> me )
{
    if ( me.ok() ) {
	delete me.ptr();
        List<void*>::remove(me); 
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
