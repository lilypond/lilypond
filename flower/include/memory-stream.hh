/*
  memory-stream.hh -- declare Memory_out_stream

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
