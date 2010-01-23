/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <cassert>
#include <cstring>
#include <cstdlib>
using namespace std;

#include "memory-stream.hh"

/*
  TODO: add read support as well.
*/
const int Memory_out_stream::block_size_ = 1024;

lily_cookie_io_functions_t
Memory_out_stream::functions_
= {
  Memory_out_stream::reader,
  Memory_out_stream::writer,
  Memory_out_stream::seeker,
  Memory_out_stream::cleaner
};

int
Memory_out_stream::cleaner (void *cookie)
{
  Memory_out_stream *stream = (Memory_out_stream *) cookie;

  stream->file_ = 0;
  return 0;
}

Memory_out_stream::Memory_out_stream ()
{
  size_ = 0;
  buffer_ = 0;
  buffer_blocks_ = 0;
  file_ = 0;

#if 0
  file_ = fopencookie ((void *) this, "w", functions_);
#endif
}

Memory_out_stream::~Memory_out_stream ()
{
  if (file_)
    fclose (file_);

  free (buffer_);
}

FILE *
Memory_out_stream::get_file () const
{
  return file_;
}

ssize_t
Memory_out_stream::get_length () const
{
  return size_;
}

char const *
Memory_out_stream::get_string () const
{
  return buffer_;
}

ssize_t
Memory_out_stream::writer (void *cookie,
			   char const *buffer,
			   size_t size)
{
  Memory_out_stream *stream = (Memory_out_stream *) cookie;

  ssize_t newsize = stream->size_ + size;

  bool change = false;
  while (newsize > stream->buffer_blocks_ * block_size_)
    {
      stream->buffer_blocks_ *= 2;
      stream->buffer_blocks_ += 1;
      change = true;
    }

  if (change)
    stream->buffer_ = (char *) realloc (stream->buffer_,
					stream->buffer_blocks_ * block_size_);

  memcpy (stream->buffer_ + stream->size_, buffer, size);
  stream->size_ = newsize;

  return size;
}

ssize_t
Memory_out_stream::reader (void * /* cookie */,
			   char * /* buffer */,
			   size_t /* size */)
{
  assert (false);
  return 0;
}

int
Memory_out_stream::seeker (void *,
			   off64_t *,
			   int)
{
  assert (false);
  return 0;
}
