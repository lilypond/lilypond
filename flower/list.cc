#ifndef LIST_CC
#define LIST_CC

#include "list.hh"

template<class T>
void
List<T>::OK() const
{
    int i = size_;
    Link<T> *lp = top_;
    while (i--) {
	assert(lp);
	lp->OK();
	lp = lp->next();
    }
    assert(!lp);
     i = size_;
    lp = bottom_;
    while (i--) {
	assert(lp);
	lp->OK();
	lp = lp->previous();
    }
    assert(!lp);
}

template<class T>
Cursor<T>
List<T>::top()
{

    // ?? waarvoor is deze if ? 
    if ( top_ )			// equivalent: if ( size_ )
	{
	Link<T>* t = top_->previous();
	assert( t != top_ );	// silly link
	while ( t )
	    {
	    top_ = t;
	    t = top_->previous();
	    }
	}
				// list empty: Cursor not ok()
    return Cursor<T>( *this, top_ );
}


template<class T>
Cursor<T>
List<T>::bottom()
{
    /* wat is dit voor zooi? kan dit niet weg?

    (invarianten!)
    */
    if ( bottom_ )		// equivalent: if ( size_ )
	{
	Link<T>* b = bottom_->next();
	assert( b != bottom_ );	// silly link    
	while ( b )
	    {
	    bottom_ = b;
	    b = bottom_->next();
	    }
	}
    				// list empty: Cursor not ok()
    return Cursor<T>( *this, bottom_ );
}


// not inlined since it assumes knowledge of destructor.
template<class T>
inline void
PointerList<T>::remove( Cursor<T> me )
{
    if ( me.ok() )
	{

	delete *me;
        List<T>::remove( me ); 
	}
}




#endif
