#include "plist.hh"

// not inlined since it assumes knowledge of destructor.
template<class T>
void
PointerList<T>::remove( Cursor<T> me )
{
    if ( me.ok() ) {
	delete *me;
        List<T>::remove( me ); 
    }
}




