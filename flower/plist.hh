/*
  list.hh -- part of flowerlib

  (c) 1996 Han-Wen Nienhuys & Jan Nieuwenhuizen
*/

#ifndef PLIST_HH
#define PLIST_HH

#include "list.hh"

/// Use for list of pointers, e.g. PointerList<AbstractType*>.
template<class T>
class PointerList : public List<void *>
{
 public:
    PCursor<T> top() {  return PCursor<T> (List<void*>::top()); }
    PCursor<T> bottom() { return PCursor<T> (List<void*>::bottom()); }
    
    PointerList( const T& thing ) : List<void*>( thing ) { }
    PointerList() {}
};


/// intrusive pl. deletes pointers given to it.
template<class T>
struct IPointerList : public PointerList<T> {
    IPointerList(IPointerList&) { set_empty(); }
    IPointerList() { }
protected:
    virtual void remove( Cursor<void*> me ) { remove (PCursor<T>(me)); }
    virtual void remove( PCursor<T> me );
};
/**
  NOTE:
  The copy constructor doesn't do what you'd want:
  Since T might have a virtual ctor, we don't try to do a

    new T(*cursor)

  You have to copy this yourself, or use the macro PointerList__copy
  
  */
#define IPointerList__copy(T, to, from, op)   \
  for (PCursor<T> _pc_(from); _pc_.ok(); _pc_++)\
      to.bottom().add(_pc_->op)\
  \


template<class T>
void PL_copy(IPointerList<T*> &dst,IPointerList<T*> const&src);


#define PL_instantiate(a)  template class PointerList<a*>
#define IPL_instantiate(a) PL_instantiate(a); template class IPointerList<a*>

#include "plist.inl"

#endif
