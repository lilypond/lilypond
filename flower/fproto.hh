/*
  fproto.hh -- typenames in flowerlib

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef FPROTO_HH
#define FPROTO_HH

template<class T> struct Array;
template<class T> struct sstack;
template<class T,class K> struct Assoc;
template<class T> struct List;
template<class T> struct PointerList;
template<class T> struct IPointerList;
template<class T> struct Cursor;
template<class T> struct PCursor;
template<class T> struct Link;
template<class T> struct Handle;
template<class T>struct Interval_t;
#include "real.hh"

typedef Interval_t<Real> Interval;

struct Choleski_decomposition;

struct Long_option_init;
struct Getopt_long;
struct Matrix;
struct String_data;
struct String_handle;
struct String_convert;
struct String;
struct virtual_smat;
struct Vector ;
struct Text_stream;
struct Data_file;
struct Text_db;
struct Scalar;
typedef unsigned char Byte;
#endif // FPROTO_HH

