/*   
  cons.hh -- declare LISP like datatypes
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef CONS_HH
#define CONS_HH


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


template<class T>
class Cons_list
{
public:
  Cons<T> * head_;
  Cons<T> ** tail_;
  Cons_list () { head_ =0; tail_ = &head_; }
  void append (Cons<T> *c)
    {
      assert (!c->next_);
      *tail_ = c;
      while (*tail_)
	tail_ = &(*tail_)->next_;
    }
  Cons<T> *remove_cons (Cons<T> **pp)
    {
      if (&(*pp)->next_ == tail_)
	tail_ = pp;

      return ::remove_cons (pp);
    }
  void junk ()
    {
      delete head_;
      head_ =0;
    }
  ~Cons_list () { junk (); }
};


template<class T>
void  copy_killing_cons_list (Cons_list<T>&, Cons<T> *src);
template<class T>
void
clone_killing_cons_list (Cons_list<T>&, Cons<T> *src);


#endif /* CONS_HH */

