/*
  list.tcc -- implement List<T>

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#ifndef LIST_CC
#define LIST_CC


#include "list.hh"

template<class T>
List<T>::List (List const&src)
{
  set_empty();
  // probably el stupido
  for (Cursor<T> c (src); c.ok(); c++)
	bottom().add (c);
}

template<class T>
void
List<T>::OK() const
{
  int i = size_;
  Link<T> *lp = top_;
  while (i--) 
    {
	assert (lp);
	lp->OK();
	lp = lp->next();
    }
  assert (!lp);
   i = size_;
  lp = bottom_;
  while (i--) 
    {
	assert (lp);
	lp->OK();
	lp = lp->previous();
    }
  assert (!lp);
}

template<class T>
void
List<T>::junk_links()
{
  Cursor<T> c (*this);
  while (c.ok())
	c.del();
}

template<class T>
List<T>::~List()
{
  junk_links();
}

/** 

  add after after_me.

  Procedure:
  \begin{itemize}
  \item if #after_me# is #ok()#, add after #after_me#, else
  \item if list !empty simply add to bottom, else
  \item list is empty: create first \Ref{Link} and initialize 
  #bottom_# and #top_#.
  \end{itemize}
*/
template<class T>
void
List<T>::add (T const & thing, Cursor<T> &after_me)
{
  if (!size_) {		// not much choice if list is empty
      bottom_ = top_ = new Link<T>(thing);
	if (!after_me.ok())
	    after_me = bottom();
    }
  else {			// add at aprioprate place
	if (!after_me.ok())
	    after_me = bottom();
	Link<T> *p =after_me.pointer();
	p->add (thing);
	if (p == bottom_)	// adjust bottom_ if necessary.
	    bottom_ = p->next();
    }

  size_++;
}

template<class T>
void
List<T>::insert (T const & thing, Cursor<T> &before_me)
{
  if (!size_) 
    {
	bottom_ = top_ = new Link<T>(thing);
	if (!before_me.ok())
	    before_me = top();
	
    }
  else 
    {
	if (!before_me.ok())
	    before_me = top();
	
	Link<T> *p = before_me.pointer() ;

	p->insert (thing);
	if (p == top_)
	    top_ = p->previous();
    }

  size_++;
}


template<class T>
void
List<T>::concatenate (List<T> const&s)
{
  Cursor<T> b (bottom());
  for (Cursor<T> c (s); c.ok(); c++) 
    {
	b.add (c);
	b++;
    }
}

#ifndef __CYGWIN32__ // ugh should check for some gcc/egcs version

// instantiate a template:  explicit instantiation.
#define LIST_INSTANTIATE(a)  template class List<a>; \
  template class Cursor<a>; template class Link<a>

#else

#define LIST_INSTANTIATE(T)\
    static void force_list_members ()\
    {\
    List<T> bla;\
    bla.top().add ((void*)0);\
    }
#endif

#endif

