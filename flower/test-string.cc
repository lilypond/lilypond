#define HAVE_BOOST_LAMBDA 1
#include "std-vector.hh"

#include <iostream>

#include "yaffut.hh"

using std::string;
using std::vector;

FUNC (string_split_join)
{
  string orig = "a/bbbb/cc//d";
  vector<string> splits = string_split (orig, '/');
  string loop = string_join (splits, "/");
  EQUAL (orig, loop);
  EQUAL (splits.size (), size_t (5));
}
