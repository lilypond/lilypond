/*
  list.hh -- part of flowerlib

  (c) 1996 Han-Wen Nienhuys & Jan Nieuwenhuizen
*/

#ifndef PLIST_HH
#define PLIST_HH

#include "list.hh"

/**
  A list of pointers.
  
  Use for list of pointers, e.g. PointerList<AbstractType*>. 
  This class does no deletion of the pointers, but it knows how to
  copy itself (shallow copy). We could have derived it from List<T>,
  but this design saves a lot of code dup; for all PointerLists in the
  program only one parent List<void*> is instantiated.
  */
template<class T>
class PointerList : public List<void *>
{
 public:
    PCursor<T> top() const{
	return PCursor<T> (List<void*>::top());
    }
    PCursor<T> bottom() const {
	return PCursor<T> (List<void*>::bottom());
    }
    PCursor<T> find(T) const;
    void concatenate(PointerList<T> const &s) { List<void*>::concatenate(s); }
    PointerList() {}
};

/**   PointerList which deletes pointers given to it. 
  NOTE:
  
  The copy constructor doesn't do what you'd want:
  Since T might have a virtual ctor, we don't try to do a

    new T(*cursor)

  You have to copy this yourself, or use the macro PointerList__copy
  
  */
template<class T>
class IPointerList : public PointerList<T> {
public:
    IPointerList(IPointerList const &) { set_empty(); }
    IPointerList() { }
    ~IPointerList();
};

#define IPointerList__copy(T, to, from, op)   \
  for (PCursor<T> _pc_(from); _pc_.ok(); _pc_++)\
      to.bottom().add(_pc_->op)\
  \


template<class T>
void PL_copy(IPointerList<T*> &dst,IPointerList<T*> const&src);


#define PL_instantiate(a)  template class PointerList<a*>; \
	template class PCursor<a*>;
#define IPL_instantiate(a) PL_instantiate(a); \
	template class IPointerList<a*>

#include "plist.icc"

#endif
