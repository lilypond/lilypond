/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2009 Han-Wen Nienhuys

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef FLOWER_PROTO_HH
#define FLOWER_PROTO_HH

char const *flower_version_str0 ();

typedef unsigned char Byte;
typedef long long I64;
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

typedef unsigned char U8;
typedef short I16;
typedef unsigned short U16;
typedef unsigned U32;
typedef int I32;
typedef unsigned long long U64;

/* We should really use LLONG_MAX; unfortunately, this appears not to
   be defined for the gub x-compiler.
*/
const U64 U64_MAX = (~0ULL);

struct File_storage;
struct Mapped_file_storage;
struct Simple_file_storage;

#endif /* FLOWER_PROTO_HH */
