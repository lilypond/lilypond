/*
  list.hh -- part of flowerlib

  (c) 1996 Han-Wen Nienhuys & Jan Nieuwenhuizen
*/

#ifndef PLIST_HH
#define PLIST_HH

#include "list.hh"

/**
  A list of pointers.
  
  Use for list of pointers, e.g. Link_list<AbstractType*>. 
  This class does no deletion of the pointers, but it knows how to
  copy itself (shallow copy). We could have derived it from List<T>,
  but this design saves a lot of code dup; for all Link_lists in the
  program only one parent List<void*> is instantiated.
  */
template<class T>
class Link_list : public List<void *>
{
 public:
    PCursor<T> top() const{
	return PCursor<T> (List<void*>::top());
    }
    PCursor<T> bottom() const {
	return PCursor<T> (List<void*>::bottom());
    }
    PCursor<T> find(T) const;
    void concatenate(Link_list<T> const &s) { List<void*>::concatenate(s); }
    Link_list() {}
};

/**   Link_list which deletes pointers given to it. 
  NOTE:
  
  The copy constructor doesn't do what you'd want:
  Since T might have a virtual ctor, we don't try to do a

    new T(*cursor)

  You have to copy this yourself, or use the macro Link_list__copy
  
  */
template<class T>
class Pointer_list : public Link_list<T> {
public:
    Pointer_list(Pointer_list const &) { set_empty(); }
    Pointer_list() { }
    ~Pointer_list();
};

#define Pointer_list__copy(T, to, from, op)   \
  for (PCursor<T> _pc_(from); _pc_.ok(); _pc_++)\
      to.bottom().add(_pc_->op)\
  \


template<class T>
void PL_copy(Pointer_list<T*> &dst,Pointer_list<T*> const&src);



#include "plist.icc"

#endif
