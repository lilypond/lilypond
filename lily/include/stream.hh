/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

