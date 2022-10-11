#ifndef __YAFFUT_PARAMETERS_H__
#define __YAFFUT_PARAMETERS_H__

#include "yaffut.hh"

#define TEST_PARAMETER(Suite, Case, Type, value)                               \
  namespace                                                                    \
  {                                                                            \
  struct Case : public yaffut::Test<Suite, Case>                               \
  {                                                                            \
    Type parameter_one_;                                                       \
    Case ();                                                                   \
  };                                                                           \
  }                                                                            \
  template struct yaffut::Test<Suite, Case>;                                   \
  Case::Case ()                                                                \
    : Suite (value),                                                           \
      parameter_one_ (value)

#define TEST_STRING(Suite, Case, String)                                       \
  TEST_PARAMETER (Suite, Case, std::string, String)

#endif // __YAFFUT_PARAMETERS_H__
