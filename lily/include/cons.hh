/*   
  cons.hh -- declare LISP like datatypes
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef CONS_HH
#define CONS_HH
#if 0 
template<class T, class U>
class NCons
{
public:
  T car_;
  U cdr_;
  NCons (T t, U u) : car_ (t), cdr_ (u) {}
  virtual ~NCons () {}
};

template<class T>
class Pointer_cons : public NCons<T, NCons*>
{
  Pointer_cons () : Cons<T, Cons*> (0,0){}
  Pointer_cons (T t, Pointer_cons<T>*c)
    : Cons<T, Cons*> (t,c)
    {
      car_ = t;
      next_ = c;
    }
};
#endif 


template<class T>
class Cons
{
public:
  T * car_;
  Cons * next_;
  Cons () {
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


template<class T>
class Cons_list
{
public:
  Cons<T> * head_;
  Cons<T> ** tail_;
  Cons_list () { head_ =0; tail_ = &head_; }
};


template<class T>
Cons_list<T> copy_killing_cons_list (Cons<T> *src);
template<class T>
Cons_list<T> clone_killing_cons_list (Cons<T> *src);


#endif /* CONS_HH */

