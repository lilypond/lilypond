/*
  list.hh -- part of flowerlib

  (c) 1996 Han-Wen Nienhuys & Jan Nieuwenhuizen
*/

#ifndef PLIST_HH
#define PLIST_HH

#include "list.hh"

/**
  A list of pointers.
  
  Use for list of pointers, e.g. Pointer_list<AbstractType*>. 
  This class does no deletion of the pointers, but it knows how to
  copy itself (shallow copy). We could have derived it from List<T>,
  but this design saves a lot of code dup; for all Pointer_lists in the
  program only one parent List<void*> is instantiated.
  */
template<class T>
class Pointer_list : public List<void *>
{
 public:
    PCursor<T> top() const{
	return PCursor<T> (List<void*>::top());
    }
    PCursor<T> bottom() const {
	return PCursor<T> (List<void*>::bottom());
    }
    PCursor<T> find(T) const;
    void concatenate(Pointer_list<T> const &s) { List<void*>::concatenate(s); }
    Pointer_list() {}
};

/**   Pointer_list which deletes pointers given to it. 
  NOTE:
  
  The copy constructor doesn't do what you'd want:
  Since T might have a virtual ctor, we don't try to do a

    new T(*cursor)

  You have to copy this yourself, or use the macro Pointer_list__copy
  
  */
template<class T>
class IPointer_list : public Pointer_list<T> {
public:
    IPointer_list(IPointer_list const &) { set_empty(); }
    IPointer_list() { }
    ~IPointer_list();
};

#define IPointer_list__copy(T, to, from, op)   \
  for (PCursor<T> _pc_(from); _pc_.ok(); _pc_++)\
      to.bottom().add(_pc_->op)\
  \


template<class T>
void PL_copy(IPointer_list<T*> &dst,IPointer_list<T*> const&src);



#include "plist.icc"

#endif
