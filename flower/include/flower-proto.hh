
/*
  fproto.hh -- typenames in flowerlib

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef FPROTO_HH
#define FPROTO_HH


char const * flower_version_sz();

template<class T> struct Link_array;
template<class T> struct Array;
template<class T,class K> struct Assoc;
template<class K, class V> struct Hash_table;
template<class K, class V> struct Hash_table_iter;
template<class T> struct Dictionary_iter;
template<class T> struct Dictionary;
template<class T> struct Dictionary_iter;
template<class T> struct Link_list;
template<class T> struct Interval_t;
template<class T> struct PQueue;

#include "real.hh"

typedef Interval_t<Real> Interval;
typedef Interval_t<int> Slice;	// junkme.

struct Offset;
struct Long_option_init;
struct Rational;
struct File_path;
struct Getopt_long;
struct String_data;
struct String_handle;
struct String_convert;
struct String;

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

typedef unsigned char Byte;


#endif // FPROTO_HH

