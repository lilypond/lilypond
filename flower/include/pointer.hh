/*
  pointer.hh -- declare P

  source file of the Flower Library

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef POINTER_HH
#define POINTER_HH

/** P<T> is a handy template to use iso T*. It inits to 0, deletes on
  destruction, deep copies upon copying

  It is probably not feasible to use P<T> as template argument, since
  a lot of auto conversion wouldn't work. new T would be called too
  much anyway.
  
  Sorry for the silly naming */
template <class T>
class P {
    
    void copy(T*);
    T* t_p;
    void junk();
public:
    
    P(P const &src);
    
    T *get_p() { T*p = t_p; t_p=0; return p; }
    T *get_l() { return t_p; }
    T *copy_p() const;
    void set_p (T *new_p); 
    void set_l (T *t_l); 
    
    P &operator =(P const &);
    ~P();
    P() { t_p = 0; }
    //P(T *p) { t_p = p; }
    
    T *operator ->() { return t_p; }
    operator T * () {  return t_p; }
    const T *operator ->() const { return t_p ; }
    operator const T *() const { return t_p; }
};
#endif // POINTER_HH

    
