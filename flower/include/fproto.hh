/*
  fproto.hh -- typenames in flowerlib

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef FPROTO_HH
#define FPROTO_HH


char const * flower_version_sz();

// what the F*** is "int" ?
// deprecate int, long, etc., use i32, i64, remember: linux-16/linux-64 ?
/// (i32)
typedef int i32;
/// (i64)
typedef long long I64;

template<class T> struct Link_array;
template<class T> struct Array;
template<class T> struct sstack;
template<class T,class K> struct Assoc;
template<class T> struct Dictionary;
template<class T> struct Dictionary_iter;
template<class T> struct List;
template<class T> struct Link_list;
template<class T> struct Pointer_list;
template<class T> struct Cursor;
template<class T> struct PCursor;
template<class T> struct Link;
template<class T> struct Handle;
template<class T> struct Interval_t;
template<class T> struct PQueue;

#include "real.hh"

typedef Interval_t<Real> Interval;
typedef Interval_t<int> Slice;

struct Choleski_decomposition;

struct Long_option_init;
struct File_path;
struct Directed_graph_node;
struct Getopt_long;
struct Matrix;
struct String_data;
struct FlowerString;
struct String_handle;
struct String_convert;
struct String;
struct Matrix_storage;
struct Vector ;
struct MyRational;


#if PARANOIA
#ifndef Rational
#define Rational MyRational
#endif
#else
struct Rational;
#endif

struct Text_stream;
struct Data_file;
struct Text_db;
struct Scalar;
typedef unsigned char Byte;


#endif // FPROTO_HH

