/*
  tex-stream.cc -- implement Tex_stream

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>

*/

#include <fstream.h>
#include <time.h>

#include "main.hh"
#include "tex-stream.hh"
#include "debug.hh"

Tex_stream::Tex_stream (String filename)
  : Paper_stream (filename)
{
  header ();
}

Tex_stream::~Tex_stream ()
{
  *os << "\n\\EndLilyPondOutput";
}

void
Tex_stream::header ()
{
  // urg, merge with Ps
  *os << _ ("% Creator: ");
  if (no_timestamps_global_b)
    *os << "GNU LilyPond\n";
  else
    *os << get_version_str () << '\n';
  *os << _ ("% Automatically generated");
  if (no_timestamps_global_b)
    *os << ".\n";
  else
    {
      *os << _ (", at ");
      time_t t (time (0));
      *os << ctime (&t) << "%\n";
    }
}

// print string. don't forget indent.
Paper_stream&
Tex_stream::operator << (Scalar s)
{
  return Paper_stream::operator << (s);
}

