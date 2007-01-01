#ifndef __YAFFUT_PARAMETERS_H__
#define __YAFFUT_PARAMETERS_H__

#include "yaffut.hh"

namespace yaffut {
template <typename Suite, typename ParameterOne, typename Case>
struct TestOne: public ITest, public Suite
{
  ParameterOne const parameter_one_;
  //static Registrator<Suite, Case> s_Registrator;
  TestOne(ParameterOne p)
  : Suite(p)
  , parameter_one_ (p)
  {
    Registrator<Suite, Case>* r = &Test<Suite, Case>::s_Registrator;
    r = 0;
  }
};

#define TEST_STRING(Suite, Case, String)\
  namespace { \
      struct Case: public yaffut::TestOne<Suite, std::string, Case>{ Case(); };	\
  } \
  template struct yaffut::TestOne<Suite, std::string, Case>; Case::Case() \
    : yaffut::TestOne<Suite, std::string, Case> (String)

}

#endif // __YAFFUT_PARAMETERS_H__
