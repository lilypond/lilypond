/*
  flowerlib

  (c) 1996 Han-Wen Nienhuys
  */
#ifndef COMPARE_HH
#define COMPARE_HH

#define ONE_OPERATOR(type, function, op) \
inline bool \
operator op (type t1, type t2) {\
    return function (t1, t2) op 0;\
}\

#define GPP_MINMAX_OPERATOR(type, op, opp) \
inline type \
operator op (type t1, type t2)\
{\
     return (t1 opp t2) ? t1 :  t2;\
}\


#if defined (__GNUG__) && ! defined (__STRICT_ANSI__)
#define GPP_MINMAX(type, prefix)\
       prefix GPP_MINMAX_OPERATOR (type, <?, <)\
       prefix GPP_MINMAX_OPERATOR (type, >?, >)
#else
#define GPP_MINMAX(type, prefix)
#endif

/**  handy notations for a signed comparison. 
    make the operators{<,<=,==,>=,>} and the MAX and MIN of two.
    Please fill a & in the type argument if necessary.    
    */
#define TEMPLATE_INSTANTIATE_COMPARE(type, function, prefix) \
prefix ONE_OPERATOR (type, function, >)\
prefix ONE_OPERATOR (type, function, >=)\
prefix ONE_OPERATOR (type, function, ==)\
prefix ONE_OPERATOR (type, function, !=)\
prefix ONE_OPERATOR (type, function, <)\
prefix ONE_OPERATOR (type, function, <=)\
GPP_MINMAX (type, prefix)\
prefix inline type max (type t1, type t2) {  return (t1 > t2)? t1 : t2; }\
prefix inline type min (type t1, type t2) {  return (t1 < t2)? t1 : t2; }\
  \
prefix  bool operator< (type t1, type t2) /* stupid fix to allow ; */


    
#define INSTANTIATE_COMPARE(type, func) TEMPLATE_INSTANTIATE_COMPARE (type,func,)
     

     
#endif
     
