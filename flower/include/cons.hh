/*   
  cons.hh -- declare LISP like datatypes
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef CONS_HH
#define CONS_HH

#include <assert.h>
#define TAIL

template<class T>
class Cons
{
public:
  T * car_p_;
  Cons * next_cons_p_;
  Cons ()
    {
      car_p_=0;
      next_cons_p_ =0;
    }
  Cons (T*t, Cons<T>*c)
    {
      car_p_ = t;
      next_cons_p_ = c;
    }
 virtual ~Cons ()
    {
      delete next_cons_p_;
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
Cons<T> *remove_cons_p (Cons<T> **pp)
{
  Cons<T> *knip = *pp;
  *pp = (*pp)->next_cons_p_;
  knip->next_cons_p_ = 0;
  return knip;
}


template<class T> int cons_list_size_i (Cons<T> *l)
{
  int i=0;
  while  (l)
    {
      l = l->next_cons_p_;
	i++;
    }
  return i;
}

/**

   Invariants:

   (*loose_cons_p_p_) is either the head_cons_p_ pointer, or a next_cons_p_ pointer from the list.
   
   **loose_cons_p_p_ == NULL
 */

template<class T>
class Cons_list
{
#ifdef TAIL
private:
  // find tail helper; is this handy?
  Cons<T> * tail_cons_l_;
#endif
  
public:
  // make these private?
  Cons<T> * head_cons_p_;
  Cons<T> ** loose_cons_p_p_;


  Cons_list ()
    {
      init ();
    }
  void init ()
    {
      head_cons_p_ = 0;
      loose_cons_p_p_ = &head_cons_p_;
#ifdef TAIL
      tail_cons_l_ = 0;
#endif
    }
  void append (Cons<T> *c)
    {
      assert (!c->next_cons_p_);
#ifndef TAIL
      *loose_cons_p_p_ = c;
      while (*loose_cons_p_p_)
	loose_cons_p_p_ = &(*loose_cons_p_p_)->next_cons_p_;
#else
      *loose_cons_p_p_ = c;
      tail_cons_l_ = *loose_cons_p_p_;
      while (tail_cons_l_->next_cons_p_)
	tail_cons_l_ = tail_cons_l_->next_cons_p_;
      loose_cons_p_p_ = &tail_cons_l_->next_cons_p_;
#endif
    }
  Cons<T>* tail_cons_l ()
    {
      assert (!empty_b ());
#ifndef TAIL
      Cons<T>* tail_cons_l = head_cons_p_;
      while (tail_cons_l->next_cons_p_)
	tail_cons_l = tail_cons_l->next_cons_p_;
      return tail_cons_l;
#else
      return tail_cons_l_;
#endif
    }
  /**
     PRE: *pp should either be the head_cons_p_ pointer,
     or the next_cons_p_ pointer from a list cell.
  */
  Cons<T> *remove_cons_p (Cons<T> **pp)
    {
#ifndef TAIL
      if (&(*pp)->next_cons_p_ == loose_cons_p_p_)
	loose_cons_p_p_ = pp;
#else
      if (*pp == tail_cons_l_)
	{
	  //either here
	  tail_cons_l_ = tail_cons_l ();
	  loose_cons_p_p_ = pp;
	}
#endif

      return ::remove_cons_p (pp);
    }
  bool empty_b ()
    {
      return !head_cons_p_;
    }
  int size_i ()
    {
      return cons_list_size_i (head_cons_p_);
    }
  T* head_car_l ()
    {
      assert (!empty_b ());
      return head_cons_p_->car_p_;
    }
  T* car_l ()
    {
      assert (!empty_b ());
      return head_cons_p_->car_p_;
    }
  T* tail_car_l ()
    {
      assert (!empty_b ());
      // or here?
#ifndef TAIL
      return tail_cons_l ()->car_p_;
#else
      return tail_cons_l_->car_p_;
#endif
    }
  void junk ()
    {
      delete head_cons_p_;
      init ();
    }
  ~Cons_list ()
    {
      junk ();
    }
};


template<class T>
void  copy_killing_cons_list (Cons_list<T>&, Cons<T> *src);
template<class T>
void
clone_killing_cons_list (Cons_list<T>&, Cons<T> *src);


#endif /* CONS_HH */

