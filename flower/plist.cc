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
