/*   
  cons.hh -- declare 
  
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
  virtual ~Cons ();
  Cons () {
    car_=0;
    next_ =0;
  }
  Cons (T*t, Cons<T>*c)
    {
      car_ = t;
      next_ = c;
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

#endif /* CONS_HH */

