/*
  flowerlib

  (c) 1996 Han-Wen Nienhuys
  */
#ifndef COMPARE_HH
#define COMPARE_HH


#define one_operator(type, function, op)        \
inline bool operator op (type t1, type t2) { return function(t1, t2) op 0; }	

/// handy notations for a signed comparison
#define template_instantiate_compare(type, function, prefix)				\
prefix one_operator(type, function, >)\
prefix one_operator(type, function, >=)\
prefix one_operator(type, function, ==)\
prefix one_operator(type, function, !=)\
prefix one_operator(type, function, <)\
prefix one_operator(type, function, <=)\
prefix inline type MAX(type t1, type t2) {  return (t1 > t2 )? t1 : t2; }\
prefix inline type MIN(type t1, type t2) {  return (t1 < t2 )? t1 : t2; }\
  \
prefix  bool operator<(type t1, type t2) /* stupid fix to allow ; */
     /**
    make the operators{<,<=,==,>=,>} and the MAX and MIN of two.
    Please fill a & in the type argument if necessary.    
    */


    
#define instantiate_compare(type, func) template_instantiate_compare(type,func, )
     

     
#endif
     
