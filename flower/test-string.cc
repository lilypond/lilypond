#define STD_VECTOR 1

#define HAVE_BOOST_LAMBDA 1
#include "std-vector.hh"

#include <iostream>

#include "yaffut.hh"

#if !STD_VECTOR
#define vector flower_vector
#endif

using namespace std;


FUNC (string_split_join)
{
  string orig = "a/bbbb/cc//d";
  vector<string> splits = string_split (orig, '/');
  string loop = string_join (splits, "/");
  EQUAL(orig, loop);
  EQUAL(splits.size (), size_t (5));
}
