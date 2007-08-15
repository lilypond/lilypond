/*
  fflower-proto.hh -- typenames in flowerlib

  (c) 1996--2007 Han-Wen Nienhuys
*/

#ifndef FLOWER_PROTO_HH
#define FLOWER_PROTO_HH

char const *flower_version_str0 ();

typedef unsigned char Byte;
struct String_convert;

#include "std-string.hh"
using namespace std;

#include "real.hh"

template<class T> struct Interval_t;
template<class T> struct PQueue;
template<class T, class A> class Matrix;

typedef Interval_t<Real> Interval;

struct Offset;
struct Long_option_init;
struct Rational;
class File_name;
class File_path;
struct Getopt_long;

struct Text_stream;
struct Data_file;
struct Text_db;
struct Scalar;

typedef unsigned char U8;
typedef short I16;
typedef unsigned short U16;
typedef unsigned U32;
typedef int I32;
typedef long long I64;


struct Duration;
struct Duration_iterator;
struct Source_file;
struct Binary_source_file;
struct Sources;
struct File_storage;
struct Mapped_file_storage;
struct Simple_file_storage;

#endif /* FLOWER_PROTO_HH */
