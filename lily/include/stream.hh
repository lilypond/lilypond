/*   
stream.hh -- declare compatibility glue for gcc 3.

source file of the GNU LilyPond music typesetter

(c) 2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#ifndef STREAM_HH
#define STREAM_HH

#include <iostream>
#include <sstream>

#include "string.hh"

#if __GNUC__ > 2
std::ostream *open_file_stream (String filename,
				std::ios_base::openmode mode=std::ios::out);
#else
std::ostream *open_file_stream (String filename, int mode=ios::out);
#endif
void close_file_stream (std::ostream *os);


#endif /* STREAM_HH */

