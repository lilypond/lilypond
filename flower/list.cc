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
inline
List<T>::~List()
{
    Cursor<T> next(*this);
    for ( Cursor<T> c( *this ); c.ok(); c = next ) {
	next = c;
	next++;
	remove( c );
    }
}

template<class T>
inline void
List<T>::add( const T& thing, Cursor<T> after_me )
{
    if (!size_) {		// not much choice if list is empty
        bottom_ = top_ = new Link<T>( thing );
    } else {			// add at aprioprate place
	Link<T> *p =  ( after_me.ok() ) ?
	    after_me.pointer() : bottom().pointer();
	p->add(thing);
	if (p == bottom_)	// adjust bottom_ if necessary.
	    bottom_ = p->next();
    }

    size_++;
}
/** 

  Procedure:
  \begin{itemize}
  \item if #after_me# is #ok()#, add after #after_me#, else
  \item if list !empty simply add to bottom, else
  \item list is empty: create first \Ref{Link} and initialize 
  #bottom_# and #top_#.
  \end{itemize}
*/

template<class T>
inline void
List<T>::insert( const T& thing, Cursor<T> before_me )
{
    if (!size_) {
	bottom_ = top_ = new Link<T>( thing );
    } else {
	Link<T> *p = 
	    (before_me.ok())?
	    before_me.pointer() : top().pointer();

	p->insert(thing);
	if (p == top_)
	    top_ = p->previous();
    }
	
    size_++;

}

#endif
