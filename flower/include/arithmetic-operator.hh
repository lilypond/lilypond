/*
  arithmetic-operator.hh -- declare

  source file of the Flower Library

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef ARITHMETIC_OPERATOR_HH
#define ARITHMETIC_OPERATOR_HH

#define IMPLEMENT_ARITHMETIC_OPERATOR(type, op)	\
  inline type					\
  operator op (type a1, type const &a2)		\
  {						\
    a1 op ## = a2;				\
    return a1;					\
  }

#endif /* ARITHMETIC_OPERATOR_HH */

