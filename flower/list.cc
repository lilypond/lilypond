#ifndef LIST_CC
#define LIST_CC

#include "list.hh"

template<class T>
List<T>::List(List const&src)
{
    set_empty();
    // probably el stupido
    for (Cursor<T> c(src); c.ok(); c++)
	bottom().add(c);
}

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
#if 0 
    // ?? waarvoor is deze if ? 
    if ( top_ )			// equivalent: if ( size_ )
	{
	Link<T>* t = top_->previous();
	assert( t != top_ );	// silly link
	while ( t )
	    {
		assert(false);	// this is even more silly.
	    top_ = t;
	    t = top_->previous();
	    }
	}
#endif
    
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


#endif
