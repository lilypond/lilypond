/*
  pcursor.hh -- part of flowerlib

  (c) 1996 Han-Wen Nienhuys&Jan Nieuwenhuizen
*/

#ifndef PCURSOR_HH
#define PCURSOR_HH

#include "plist.hh"
#include "cursor.hh"

/**  cursor to go with Link_list. 
  don't create Link_list<void*>'s.
  This cursor is just an interface class for Cursor. It takes care of the
  appropriate type casts
 */
template<class T>
class PCursor : private Cursor<void *> {
  friend class Pointer_list<T>;

  /// delete contents
  void junk();
public:
  Cursor<void*>::ok;
  Cursor<void*>::del;
  Cursor<void*>::backspace;
  Cursor<void*>::next;
  Cursor<void*>::previous;

  T remove_p() {
    T p = ptr();
    Cursor<void*>::del();
    return p;
  }
  T remove_prev_p() {
    assert (ok());
    (*this)--;
    return remove_p();
  }
    
  Link_list<T> *list_l() { return (Link_list<T> *)Cursor<void*>::list_l (); }
  PCursor<T> operator++(int) { return Cursor<void*>::operator++(0);}
  PCursor<T> operator--(int) { return Cursor<void*>::operator--(0); }
  PCursor<T> operator+=(int i) { return Cursor<void*>::operator+=(i);}
  PCursor<T> operator-=(int i) { return Cursor<void*>::operator-=(i); }    
  PCursor<T> operator -(int no) const { return Cursor<void*>::operator-(no);}
  int operator -(PCursor<T> op) const { return Cursor<void*>::operator-(op);}
  PCursor<T> operator +(int no) const {return Cursor<void*>::operator+(no);}
  PCursor (const Link_list<T> & l) : Cursor<void*> (l) {}
  PCursor (const Cursor<void*>& cursor) : Cursor<void*>(cursor) { }
  void* vptr() const { return *((Cursor<void*> &) *this); }

  // should return T& ?
  T ptr() const { return (T) vptr (); }
  T operator ->() const { return  ptr(); }
  operator T() { return ptr(); }
  T operator *() { return ptr(); }
  void add (T const & p) { Cursor<void*>::add ((void*) p); }
  void insert (T const & p) { Cursor<void*>::insert ((void*) p);}    
  static int compare (PCursor<T> a,PCursor<T>b) {
    return Cursor<void*>::compare (a,b);
  }
};



#include "compare.hh"
TEMPLATE_INSTANTIATE_COMPARE(PCursor<T>, PCursor<T>::compare, template<class T>);

#endif
