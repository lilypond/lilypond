/*
  midi-stream.hh -- declare Midi_stream

  (c) 1997--2008 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef MIDI_STREAM_HH
#define MIDI_STREAM_HH

#include <cstdio>
using namespace std;

#include <string.h>
#include "std-string.hh"
#include "lily-proto.hh"

struct Midi_stream
{
  Midi_stream (string file_name_string);
  ~Midi_stream ();

  void write (string);
  void write (Midi_chunk const &);
  void open ();

  FILE *out_file_;
  string file_name_string_;
};

#endif // MIDI_STREAM_HH
