/*
  list.hh -- part of flowerlib

  (c) 1996 Han-Wen Nienhuys & Jan Nieuwenhuizen
*/

#ifndef PLIST_HH
#define PLIST_HH

#include "list.hh"

/// Use for list of pointers, e.g. PointerList<AbstractType*>.
template<class T>
class PointerList : public List<T>
{
 public:
    PointerList(PointerList&) { set_empty(); }
    PointerList( const T& thing ) : List<T>( thing ) { }
    PointerList() {}
    ///
    virtual ~PointerList();
    /**
      This function deletes deletes the allocated pointers of all links. 
      #\Ref{~List}# is used to delete the links themselves.
      */ 

 protected:
    virtual void remove( Cursor<T> me );
};
/**
  NOTE:
  The copy constructor doesn't do what you'd want:
  Since T might have a virtual ctor, we don't try to do a

    new T(*cursor)

  You have to copy this yourself, or use the macro PointerList__copy
  
  */
#define PointerList__copy(T, to, from, op)   \
  for (PCursor<T> _pc_(from); _pc_.ok(); _pc_++)\
      to.bottom().add(_pc_->op)\
  \


template<class T>
void PL_copy(PointerList<T*> &dst,PointerList<T*> const&src);

#define PL_instantiate(a) L_instantiate(a *); template class PointerList<a*>

#include "plist.inl"

#endif
