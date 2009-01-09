/*
  midi.c -- implement Python midi parser module

  source file of the GNU LilyPond music typesetter

  (c) 2001--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
            Jan Nieuwenhuizen <janneke@gnu.org>

*/

/*

python
import midi
s = open ("s.midi").read ()
midi.parse_track (s)
midi.parse (s)


returns a MIDI file as the tuple

 (format, division, TRACKLIST) 

each track is an EVENTLIST, where EVENT is

  (time, (type, ARG1, [ARG2]))

*/

#include <Python.h>

/* PyMIDINIT_FUNC isn't defined in Python < 2.3 */
#ifndef PyMODINIT_FUNC
#       if defined(__cplusplus)
#               define PyMODINIT_FUNC extern "C" void
#       else /* __cplusplus */
#               define PyMODINIT_FUNC void
#       endif /* __cplusplus */
#endif

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
midi_error (char const *func, char *s)
{
  char *dest = (char*) malloc (sizeof (char) * (strlen (func) + strlen (s) + 1));
  strcpy (dest, func);
  strcat (dest, s);
  PyErr_SetString (Midi_error, dest);
  free (dest);
  
  return 0;
}

static PyObject *
midi_warning (char const *s)
{
  PyErr_SetString (Midi_warning, s);
  return 0;
}


typedef struct message {
  unsigned char msg;
  char * description;
} message_t;

message_t channelVoiceMessages[] = {
  {0x80, "NOTE_OFF"},
  {0x90, "NOTE_ON"},
  {0xA0, "POLYPHONIC_KEY_PRESSURE"},
  {0xB0, "CONTROLLER_CHANGE"},
  {0xC0, "PROGRAM_CHANGE"},
  {0xD0, "CHANNEL_KEY_PRESSURE"},
  {0xE0, "PITCH_BEND"},
  {0,0}
};

message_t channelModeMessages[] = {
  {0x78, "ALL_SOUND_OFF"},
  {0x79, "RESET_ALL_CONTROLLERS"},
  {0x7A, "LOCAL_CONTROL"},
  {0x7B, "ALL_NOTES_OFF"},
  {0x7C, "OMNI_MODE_OFF"},
  {0x7D, "OMNI_MODE_ON"},
  {0x7E, "MONO_MODE_ON"},
  {0x7F, "POLY_MODE_ON"},
  {0,0}
};

message_t metaEvents[] = {
  {0x00, "SEQUENCE_NUMBER"},
  {0x01, "TEXT_EVENT"},
  {0x02, "COPYRIGHT_NOTICE"},
  {0x03, "SEQUENCE_TRACK_NAME"},
  {0x04, "INSTRUMENT_NAME"},
  {0x05, "LYRIC"},
  {0x06, "MARKER"},
  {0x07, "CUE_POINT"},
  {0x20, "MIDI_CHANNEL_PREFIX"},
  {0x21, "MIDI_PORT"},
  {0x2F, "END_OF_TRACK"},
  {0x51, "SET_TEMPO"},
  {0x54, "SMTPE_OFFSET"},
  {0x58, "TIME_SIGNATURE"},
  {0x59, "KEY_SIGNATURE"},
  {0x7F, "SEQUENCER_SPECIFIC_META_EVENT"},
  {0xFF, "META_EVENT"},
  {0,0}
};

void
add_constants (PyObject *dict)
{
  message_t * p[] = {metaEvents, channelModeMessages, channelVoiceMessages ,0};
  int i,j;
  for ( j =0; p[j]; j++)
    for ( i = 0; p[j][i].description; i++)
      PyDict_SetItemString (dict, p[j][i].description, Py_BuildValue ("i", p[j][i].msg));
}

unsigned long int
get_number (unsigned char ** str, unsigned char * end_str, int length)
{
  /* # MIDI uses big-endian for everything */
  long sum = 0;
  int i = 0;
  
  for (; i < length &&
	 ((*str) + i < end_str); i++)
    sum = (sum << 8) + (unsigned char) (*str)[i];

  *str += length;
  debug_print ("%d:\n", sum);
  return sum;
}

unsigned long int
get_variable_length_number (unsigned char **str, unsigned char * end_str)
{
  long sum = 0;

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

PyObject *
read_one_byte (unsigned char **track, unsigned char *end, 
	       unsigned char x)
{
  PyObject *pyev = Py_BuildValue ("(i)", x);
  debug_print ("%x:%s", x, "event\n");

  return pyev;
}

PyObject *
read_two_bytes (unsigned char **track, unsigned char *end, 
		unsigned char x)
{
  PyObject *pyev = Py_BuildValue ("(ii)", x, (*track)[0]);
  *track += 1;
  debug_print ("%x:%s", x, "event\n");
  return pyev;
}

PyObject *
read_three_bytes (unsigned char **track, unsigned char *end, 
		  unsigned char x)
{
  PyObject *pyev = Py_BuildValue ("(iii)", x, (*track)[0],
				  (*track)[1]);

  *track += 2;
  debug_print ("%x:%s", x, "event\n");
  return pyev;
}

PyObject *
read_string (unsigned char **track, unsigned char *end) 
{
  unsigned long length = get_variable_length_number (track, end);
  if (length > end - *track)
    length = end - *track;

  *track += length;
  return Py_BuildValue ("s#", ((*track) -length), length);
}

typedef PyObject* (*Read_midi_event)
  (unsigned char **track, unsigned char *end, 
   unsigned char x);


static PyObject *
read_f0_byte (unsigned char **track, unsigned char *end, 
	      unsigned char x)
	      
{
  debug_print ("%x:%s", x, "event\n");
  if (x == 0xff)
    {
      unsigned char z = (*track)[0 ];
      *track += 1;
      debug_print ("%x:%s", z, "f0-event\n");

      return Py_BuildValue ("(iiO)", x, z, read_string (track, end));
    }

  return Py_BuildValue ("(iO)", x, read_string (track, end));
}

Read_midi_event read_midi_event [16] =
{
  read_one_byte,  //  0
  read_one_byte,  // 10
  read_one_byte,  // 20
  read_one_byte,  // 30
  read_one_byte,  // 40
  read_one_byte,  // 50
  read_one_byte,  // 60 data entry.
  read_two_bytes, // 70 all notes off
  read_three_bytes, // 80 note off
  read_three_bytes, // 90 note on
  read_three_bytes, // a0 poly aftertouch
  read_three_bytes, // b0 control
  read_two_bytes,  // c0 prog change
  read_two_bytes, // d0 ch aftertouch
  read_three_bytes, // e0 pitchwheel range 
  read_f0_byte,   // f0
};


static PyObject *
read_event (unsigned char **track, unsigned char *end, PyObject *time,
	    unsigned char *running_status)
{
  int rsb_skip = ((**track & 0x80)) ? 1 :0;

  unsigned char x = (rsb_skip) ? (*track)[0]: *running_status;

  PyObject * bare_event = 0;
  debug_print ("%x:%s", x, "event\n");
  *running_status = x;
  *track += rsb_skip;
  
  //  printf ("%x %x %d next %x\n", x, (*track)[0], rsb_skip, (*track)[1]);
  bare_event = (*read_midi_event[x >> 4]) (track, end, x);
  if (bare_event)
    return Py_BuildValue ("(OO)", time, bare_event);
  else
    return NULL;
}

static PyObject *
midi_parse_track (unsigned char **track, unsigned char *track_end)
{
  unsigned int time = 0;
  unsigned long track_len, track_size;
  PyObject *pytrack = 0;

  debug_print ("%s", "\n");
  
  track_size = track_end - *track;

  debug_print ("%s", "\n");
  if (memcmp (*track, "MTrk", 4))
    return midi_error (__FUNCTION__,  ": MTrk expected");
  
  *track += 4;

  track_len = get_number (track, *track + 4, 4);


  debug_print ("track_len: %u\n", track_len);
  debug_print ("track_size: %u\n", track_size);
  debug_print ("track begin: %p\n", track);
  debug_print ("track end: %p\n", track + track_len);
  
  if (track_len > track_size)
    return midi_error (__FUNCTION__,  ": track size corrupt");

  pytrack = PyList_New (0);

  if (*track + track_len < track_end)
    track_end = *track + track_len;

  {  
    PyObject *pytime = PyInt_FromLong (0L);
    unsigned char running_status = 0;
	
    while (*track < track_end)
      {
	long dt = get_variable_length_number(track, track_end);
	PyObject *pyev = 0;

	time += dt;
	if (dt)
	  pytime = PyInt_FromLong (time);

	pyev = read_event (track, track_end, pytime,
			   &running_status);
	if (pyev)
	  PyList_Append (pytrack, pyev);
      }
  }
  
  *track = track_end;
  return pytrack;
}


static PyObject *
pymidi_parse_track (PyObject *self, PyObject *args)
{
  unsigned char *track, *track_end;
  unsigned long track_size;

  debug_print ("%s", "\n");
  if (!PyArg_ParseTuple (args, "s#", &track, &track_size))
    return 0;

  if (track_size < 0)
    return midi_error (__FUNCTION__,   ": negative track size");

  track_end = track + track_size;
  
  return midi_parse_track (&track, track_end);
}

static PyObject *
midi_parse (unsigned char **midi,unsigned  char *midi_end)
{
  PyObject *pymidi = 0;
  unsigned long header_len;
  unsigned format, tracks;
  int division;
  int i;
  
  debug_print ("%s", "\n");

  /* Header */
  header_len = get_number (midi, *midi + 4, 4);
  
  if (header_len < 6)
    return midi_error (__FUNCTION__,  ": header too short");
    
  format = get_number (midi, *midi + 2, 2);
  tracks = get_number (midi, *midi + 2, 2);

  if (tracks > 32)
    return midi_error (__FUNCTION__,  ": too many tracks");
  
  division = get_number (midi, *midi + 2, 2) * 4;


  if (division < 0)
    /* return midi_error (cannot handle non-metrical time"); */
    ;
  *midi += header_len - 6;

  pymidi = PyList_New (0);

  /* Tracks */
  for (i = 0; i < tracks; i++)
    PyList_Append (pymidi, midi_parse_track (midi, midi_end));

  pymidi = Py_BuildValue ("(OO)", Py_BuildValue ("(ii)", format, division),
			  pymidi);
  return pymidi;
}

static PyObject *
pymidi_parse (PyObject *self, PyObject *args)
{
  unsigned char *midi, *midi_end;
  unsigned long midi_size;
  
  debug_print ("%s", "\n");
  if (!PyArg_ParseTuple (args, "s#", &midi, &midi_size))
    return 0;

  if (memcmp (midi, "MThd", 4))
    return midi_error (__FUNCTION__,  ": MThd expected");
  
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

PyMODINIT_FUNC
initmidi (void)
{
  PyObject *m, *d;
  m = Py_InitModule ("midi", MidiMethods);
  d = PyModule_GetDict (m);
  
  Midi_error = PyString_FromString ("midi.error");
  PyDict_SetItemString (d, "error", Midi_error);
  add_constants (d);
  Midi_warning = PyString_FromString ("midi.warning");
  PyDict_SetItemString (d, "warning", Midi_warning);

  /*
    FIXME.
   */
  (void) midi_warning;
}
 
