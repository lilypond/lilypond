/*   
  cons.hh -- declare LISP like datatypes
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef CONS_HH
#define CONS_HH


#include <assert.h>

template<class T>
class Cons
{
public:
  T * car_;
  Cons * next_;
  Cons ()
    {
      car_=0;
      next_ =0;
    }
  Cons (T*t, Cons<T>*c)
    {
      car_ = t;
      next_ = c;
    }
 virtual ~Cons ()
    {
      delete next_;
    }
};

template<class T>
class Killing_cons : public Cons<T>
{
public:
  Killing_cons (T *t, Cons<T> *p)
    : Cons<T>( t,p)
    {
    }
  virtual ~Killing_cons ();
};


/// remove the link pointed to by *p.
template<class T>
Cons<T> *remove_cons (Cons<T> **pp)
{
  Cons<T> *knip = *pp;
  *pp = (*pp)->next_;
  knip->next_ = 0;
  return knip;
}

template<class T> int cons_list_size_i (Cons<T> *l)
{
  int i=0;
  while  (l)
    {
      l = l->next_;
	i++;
    }
  return i;
}





template<class T>
Cons<T> * last_cons (Cons<T> * head)
{
  while (head && head->next_)
    {
      head = head->next_;
    }
  return head;
}

/**

   Invariants:

   (*tail_) is either the head_ pointer, or a next_ pointer from the list.
   
   **tail_ == NULL
 */

template<class T>
class Cons_list
{
public:
  Cons<T> * head_;
  Cons<T> ** nil_pointer_address_;
  Cons_list ()
    {
      init ();
    }
  void init ()
    {
      head_ =0;
      nil_pointer_address_ = &head_;
    }
  void append (T *c)
    {
      append (new Cons<T> (c,0));
    }
  void append (Cons<T> *c)
    {
      assert (!c->next_);
      *nil_pointer_address_ = c;
      while (*nil_pointer_address_)
	nil_pointer_address_ = &(*nil_pointer_address_)->next_;
    }
  /**
     PRE: *pp should either be the head_ pointer, or the next_ pointer
     from a list cell.
  */
  Cons<T> *remove_cons (Cons<T> **pp)
    {
      if (&(*pp)->next_ == nil_pointer_address_)
	nil_pointer_address_ = pp;

      return ::remove_cons (pp);
    }

  /// junk everything after the  first I elements.
  void truncate (int i)
    {
      Cons<T> **p  = &head_;
      for (; *p && i;  p = &((*p)->next_))
	{
	  i--;
	}

      if (*p)
	{
	  delete *p;
	  *p = 0;
	}
      nil_pointer_address_ = p;
    }

  void junk ()
    {
      delete head_;
      head_ =0;
    }
  ~Cons_list ()
    {
      junk ();
    }
  int size_i ()
    {
      return cons_list_size_i (head_);
    }
};


template<class T>
void  copy_killing_cons_list (Cons_list<T>&, Cons<T> *src);
template<class T>
void
clone_killing_cons_list (Cons_list<T>&, Cons<T> *src);

#endif /* CONS_HH */

