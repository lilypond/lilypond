/*
  tex-stream.cc -- implement Ps_stream

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <fstream.h>
#include <time.h>

#include "main.hh"
#include "ps-stream.hh"
#include "debug.hh"

Ps_stream::Ps_stream (String filename)
  : Paper_stream (filename)
{
  header ();
}

Ps_stream::~Ps_stream ()
{
  *os << "\nshowpage\n";
}

void
Ps_stream::header ()
{
  *os << _ ("%!PS-Adobe-3.0\n");
  // urg, merge with Tex
  *os << _ ("%%Creator: ");
  if (no_timestamps_global_b)
    *os << "GNU LilyPond\n";
  else
    *os << get_version_str () << '\n';
  *os << _ ("%%Automatically generated");
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
Ps_stream::operator << (Scalar s)
{
  return Paper_stream::operator << (s);
}

