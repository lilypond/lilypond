/*
  midi.c -- implement Python midi parser module

  source file of the GNU LilyPond music typesetter

  (c)  2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
            Jan Nieuwenhuizen <janneke@gnu.org>

*/

/*

python2
import midi
s = open ("s.midi").read ()
midi.parse_track (s)
midi.parse (s)

*/

#include <python2.0/Python.h>

#if 0
int x = 0;
int *track = &x;
#define debug_print(f, args...) fprintf (stderr, "%s:%d: track: %p :" f, __FUNCTION__, __LINE__, *track, ##args)
#else
#define debug_print(f, args...)
#endif

static PyObject *Midi_error;
static PyObject *Midi_warning;

static PyObject *
midi_error (char *s)
{
  PyErr_SetString (Midi_error, s);
  return 0;
}

static PyObject *
midi_warning (char *s)
{
  PyErr_SetString (Midi_warning, s);
  return 0;
}


typedef struct message {
  unsigned char msg;
  char * description;
} message_t;

message_t channelVoiceMessages[] = {
  0x80, "NOTE_OFF",
  0x90, "NOTE_ON",
  0xA0, "POLYPHONIC_KEY_PRESSURE",
  0xB0, "CONTROLLER_CHANGE",
  0xC0, "PROGRAM_CHANGE",
  0xD0, "CHANNEL_KEY_PRESSURE",
  0xE0, "PITCH_BEND",
  0,0
};

message_t channelModeMessages[] = {
  0x78, "ALL_SOUND_OFF",
  0x79, "RESET_ALL_CONTROLLERS",
  0x7A, "LOCAL_CONTROL",
  0x7B, "ALL_NOTES_OFF",
  0x7C, "OMNI_MODE_OFF",
  0x7D, "OMNI_MODE_ON",
  0x7E, "MONO_MODE_ON",
  0x7F, "POLY_MODE_ON",
  0,0
};

message_t metaEvents[] = {
  0x00, "SEQUENCE_NUMBER",
  0x01, "TEXT_EVENT",
  0x02, "COPYRIGHT_NOTICE",
  0x03, "SEQUENCE_TRACK_NAME",
  0x04, "INSTRUMENT_NAME",
  0x05, "LYRIC",
  0x06, "MARKER",
  0x07, "CUE_POINT",
  0x20, "MIDI_CHANNEL_PREFIX",
  0x21, "MIDI_PORT",
  0x2F, "END_OF_TRACK",
  0x51, "SET_TEMPO",
  0x54, "SMTPE_OFFSET",
  0x58, "TIME_SIGNATURE",
  0x59, "KEY_SIGNATURE",
  0x7F, "SEQUENCER_SPECIFIC_META_EVENT",
  0,0
};

unsigned long int
get_number (char * str, char * end_str, int length)
{
  /* # MIDI uses big-endian for everything */
  long sum = 0;
  int i = 0;
  
  for (; i < length; i++)
    sum = (sum << 8) + (unsigned char) str[i];

  debug_print ("%d:\n", sum);
  return sum;
}

unsigned long int
get_variable_length_number (char **str, char * end_str)
{
  long sum = 0;
  int i = 0;
  while (*str < end_str)
    {
      unsigned char x = **str;
      (*str) ++;
      sum = (sum << 7) + (x & 0x7F);
      if (!(x & 0x80))
	break;
    }
  debug_print ("%d:\n", sum);
  return sum;
}

static PyObject *
read_unimplemented_event (char **track, char *end, unsigned long time,
			  unsigned char x, unsigned char z)
{
  debug_print ("%x:%s", z, "unimplemented MIDI event\n");
  *track += 2;
  return Py_BuildValue ("(iii)", z, *((*track) -2), *((*track) -1));
}

PyObject *
read_one_byte (char **track, char *end, unsigned long time,
	       unsigned char x, unsigned char z)
{
  debug_print ("%x:%s", z, "event\n");
  *track += 1;
  return Py_BuildValue ("(ii)", z, *((*track) -1));
}

PyObject *
read_two_bytes (char **track, char *end, unsigned long time,
		unsigned char x, unsigned char z)
{
  debug_print ("%x:%s", z, "event\n");
  *track += 2;
  return Py_BuildValue ("(iii)", z, *((*track) -2), *((*track) -1));
}

PyObject *
read_three_bytes (char **track, char *end, unsigned long time,
		  unsigned char x, unsigned char z)
{
  debug_print ("%x:%s", z, "event\n");
  *track += 3;
  return Py_BuildValue ("(iiii)", z, *((*track) -3), *((*track) -2), *((*track) -1));
}

PyObject *
read_string (char **track, char *end, unsigned long time,
	     unsigned char x, unsigned char z)
{
  unsigned long length = get_variable_length_number (track, end);
  if (length < *track - end)
    debug_print ("%s", "zero length string\n");
  *track += length;
  debug_print ("%x:%s", length, "string\n");
  return Py_BuildValue ("(is)", z, *((*track) -length));
}

typedef PyObject* (*Read_midi_event)
     (char **track, char *end, unsigned long time,
      unsigned char x, unsigned char z);

Read_midi_event read_ff_byte [16] =
{
  read_three_bytes,  //  0
  read_one_byte,     // 10
  read_one_byte,  // 20
  read_one_byte,  // 30
  read_one_byte,  // 40
  read_one_byte,  // 50
  read_one_byte,  // 60
  read_two_bytes, // 70
  read_two_bytes, // 80
  read_two_bytes, // 90
  read_two_bytes, // a0
  read_two_bytes, // b0
  read_one_byte,  // c0
  read_two_bytes, // d0
  read_two_bytes, // e0
  read_two_bytes, // e0
};


static PyObject *
read_f0_byte (char **track, char *end, unsigned long time,
	      unsigned char x, unsigned char z)
	      
{
  debug_print ("%x:%s", z, "event\n");
  if (z == 0xff)
    {
      unsigned char zz = *(*track)++;
      debug_print ("%x:%s", zz, "f0-event\n");
      if (zz == 0x01 && **track <= 0x07)
	return read_string (track, end, time, x, zz);
      else if (zz == 0x2f && **track == 0x00)
	{
	  (*track)++;
	  debug_print ("%s", "done\n");
	  return 0;
	}
      else
	return read_unimplemented_event (track, end, time, x, zz);
      exit (0);
    }
  else
    return read_string (track, end, time, x, z);
}

Read_midi_event read_midi_event [16] =
{
  read_one_byte,  //  0
  read_one_byte,  // 10
  read_one_byte,  // 20
  read_one_byte,  // 30
  read_one_byte,  // 40
  read_one_byte,  // 50
  read_one_byte,  // 60
  read_two_bytes, // 70
  read_two_bytes, // 80
  read_two_bytes, // 90
  read_two_bytes, // a0
  read_two_bytes, // b0
  read_one_byte,  // c0
  read_two_bytes, // d0
  read_two_bytes, // e0
  read_f0_byte,   // f0
};
	     
static PyObject *
read_event (char **track, char *end, unsigned long time,
	    unsigned char *running_status)
{
  int rsb_skip = ((**track & 0x80)) ? 1 :0;

  unsigned char x = (rsb_skip) ? (*track)[0]: *running_status;
  // unsigned char y = x & 0xf0;
  unsigned char z = (*track)[1 + rsb_skip];
  
  debug_print ("%x:%s", z, "event\n");
  *track += 2 + rsb_skip;

  return (*read_midi_event[z >> 4]) (track, end, time, x, z);
}

static PyObject *
midi_parse_track (char **track, char *track_end)
{
  unsigned int time = 0;
  unsigned char running_status;
  unsigned long track_len, track_size;
  PyObject *pytrack = 0;

  debug_print ("%s", "\n");
  
  track_size = track_end - *track;
  /* need this for direct midi.parse_track (s) on midi file */
  if (strcmp (*track, "MTrk"))
    *track = memmem (*track, track_size - 1, "MTrk", 4);
  debug_print ("%s", "\n");
  assert (!strcmp (*track, "MTrk"));
  *track += 4;

  track_len = get_number (*track, *track + 4, 4);
  *track += 4;

  debug_print ("track_len: %u\n", track_len);
  debug_print ("track_size: %u\n", track_size);
  debug_print ("track begin: %p\n", track);
  debug_print ("track end: %p\n", track + track_len);
  
  //assert (track_len <= track_size);

  pytrack = PyList_New (0);

  track_end = *track + track_len;

  while (*track < track_end)
    {
      long dt = get_variable_length_number(track, track_end);
      time += dt;
      PyList_Append (pytrack, read_event (track, track_end, time,
					  &running_status));
    }

  *track = track_end;
  return pytrack;
}


static PyObject *
pymidi_parse_track (PyObject *self, PyObject *args)
{
  char *track, *track_end;
  unsigned long track_size, track_len;

  PyObject * sobj = PyTuple_GetItem (args, 0);

  debug_print ("%s", "\n");
  if (!PyArg_ParseTuple (args, "s#", &track, &track_size))
    return 0;

  if (track_size < 0)
    return midi_error ("negative track size");

  track_end = track + track_size;
  
  return midi_parse_track (&track, track_end);
}

static PyObject *
midi_parse (char **midi, char *midi_end)
{
  PyObject *pymidi = 0;
  unsigned long header_len;
  unsigned format, tracks;
  int division;
  int i;
  
  debug_print ("%s", "\n");

  /* Header */
  header_len = get_number (*midi, *midi + 4, 4);
  *midi += 4;
  
  if (header_len < 6)
    return midi_error ("header too short");
    
  format = get_number (*midi, *midi + 2, 2);
  *midi += 2;
  
  tracks = get_number (*midi, *midi + 2, 2);
  *midi += 2;

  if (tracks > 32)
    return midi_error ("too many tracks");
  
  division = get_number (*midi, *midi + 2, 2) * 4;
  *midi += 2;

  if (division < 0)
    /* return midi_error ("can't handle non-metrical time"); */
    ;
  *midi += header_len - 6;

  pymidi = PyList_New (0);
  PyList_Append (pymidi, Py_BuildValue ("(iii)", format, tracks, division));

  /* Tracks */
  for (i = 0; i < tracks; i++)
    PyList_Append (pymidi, midi_parse_track (midi, midi_end));

  return pymidi;
}

static PyObject *
pymidi_parse (PyObject *self, PyObject *args)
{
  char *midi, *midi_end;
  unsigned long midi_size, midi_len;
  
  PyObject *sobj = PyTuple_GetItem (args, 0);

  debug_print ("%s", "\n");
  if (!PyArg_ParseTuple (args, "s#", &midi, &midi_size))
    return 0;

  if (strcmp (midi, "MThd"))
      return midi_error ("MThd expected");
  
  midi += 4;

  midi_end = midi + midi_size;

  return midi_parse (&midi, midi_end);
}


static PyMethodDef MidiMethods[] = 
{
  {"parse",  pymidi_parse, 1},
  {"parse_track",  pymidi_parse_track, 1},
  {0, 0}        /* Sentinel */
};

initmidi ()
{
  PyObject *m, *d;
  m = Py_InitModule ("midi", MidiMethods);
  d = PyModule_GetDict (m);
  
  Midi_error = PyString_FromString ("midi.error");
  PyDict_SetItemString (d, "error", Midi_error);
  Midi_warning = PyString_FromString ("midi.warning");
  PyDict_SetItemString (d, "warning", Midi_warning);
}
