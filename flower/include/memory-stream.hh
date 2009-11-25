/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef MEMORY_STREAM_HH
#define MEMORY_STREAM_HH

#include <cstdio>
#include <unistd.h>
using namespace std;

#include "libc-extension.hh"
#include "file-cookie.hh"

class Memory_out_stream
{
  char *buffer_;
  ssize_t size_;
  int buffer_blocks_;
  FILE *file_;

  static lily_cookie_io_functions_t functions_;
  static const int block_size_;

public:
  static ssize_t reader (void *, char *, size_t);
  static ssize_t writer (void *, char const *, size_t);
  static int seeker (void *, off64_t *, int);
  static int cleaner (void *);

  Memory_out_stream ();
  ~Memory_out_stream ();
  FILE *get_file () const;
  char const *get_string () const;
  ssize_t get_length () const;
};

#endif /* MEMORY_STREAM_HH */
