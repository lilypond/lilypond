#ifndef COMPARE_HH
#define COMPARE_HH

/// handy notations for a signed comparison
#define template_instantiate_compare(type, function, prefix)				\
prefix inline bool operator>(type t1, type t2) { return function(t1, t2) > 0; }	\
prefix inline bool operator>=(type t1, type t2) { return function(t1, t2) >= 0; }	\
prefix inline bool operator==(type t1, type t2) { return function(t1, t2) == 0; }	\
prefix inline bool operator<=(type t1, type t2) { return function(t1, t2) <= 0; }	\
prefix inline bool operator<(type t1, type t2) { return function(t1, t2) < 0; } \
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
     
