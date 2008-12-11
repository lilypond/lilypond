/*
  stream.hh -- declare compatibility glue for gcc 3.

  source file of the GNU LilyPond music typesetter

  (c) 2001--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef STREAM_HH
#define STREAM_HH

#include "std-string.hh"

#include <iostream>
#include <sstream>
using namespace std;

#if __GNUC__ > 2
ostream *open_file_stream (string file_name,
				ios_base::openmode mode = ios::out);
#else
ostream *open_file_stream (string file_name, int mode = ios::out);
#endif
void close_file_stream (ostream *os);

#endif /* STREAM_HH */

