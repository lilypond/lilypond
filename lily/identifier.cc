/*
  identifier.cc -- implement identifier and derived classes

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <assert.h>
#include "midi-def.hh"
#include "paper-def.hh"
#include "score.hh"
#include "identifier.hh"
#include "my-lily-lexer.hh"
#include "debug.hh"
#include "symtable.hh"
#include "lookup.hh"
#include "script-def.hh"
#include "request.hh"
#include "translator.hh"


IMPLEMENT_IS_TYPE_B(Identifier);

Identifier::Identifier (int code)
{
  token_code_i_ = code;
  accessed_b_ = 0;
  init_b_ = 0;
}



Identifier::Identifier (Identifier const&s)
  : Input (s)
{
  token_code_i_ = s.token_code_i_;
  accessed_b_ = s.accessed_b_;
  init_b_ = s.init_b_;
}

Identifier::~Identifier()
{
}

void
Identifier::error (String expect) const
{
  String e (_("Wrong identifier type: "));
  e += String (name()) + _("(expected ") + expect + ")";
  ::error (e);
}

String
Identifier::str () const
{
  return do_str ();
}

String
Identifier::do_str () const
{
  return "";
}

void
Identifier::print () const
{
  DOUT << "identifier ";
  do_print ();
}
void
Identifier::do_print () const
{
}

/* ugh. */
/* UGH MEMORY LEAK! */
#define DEFAULT_PRINT(Class, accessor) \
void \
Class ## _identifier::do_print () const { \
  Class *cl = ((Class ## _identifier *)this)->accessor();\
  cl->print (); \
  delete cl; \
}



DEFAULT_PRINT(General_script_def, script);
DEFAULT_PRINT(Lookup, lookup);
DEFAULT_PRINT(Translator, translator);
DEFAULT_PRINT(Symtables, symtables);
DEFAULT_PRINT(Music, music);
DEFAULT_PRINT(Request, request);
DEFAULT_PRINT(Score, score);
DEFAULT_PRINT(Midi_def, mididef);
DEFAULT_PRINT(Paper_def, paperdef);

/* ugh. */
#define DUMMY_STR(Class) \
String \
Class ## _identifier::do_str () const { \
  return String (#Class); \
}



DUMMY_STR(General_script_def);
DUMMY_STR(Lookup);
DUMMY_STR(Translator);
DUMMY_STR(Symtables);
DUMMY_STR(Music);
DUMMY_STR(Request);
DUMMY_STR(Score);
DUMMY_STR(Midi_def);
DUMMY_STR(Paper_def);
DUMMY_STR(Duration);

#define STRING_PRINT(Class) \
void \
Class ## _identifier::do_print () const\
{\
  DOUT << do_str () << "\n";\
}\


STRING_PRINT(Duration);
STRING_PRINT(Real);
STRING_PRINT(int);
STRING_PRINT(String);

#define DEFAULT_STR(Class) \
String \
Class ## _identifier::do_str () const\
{\
  return String (*data_p_);\
}

DEFAULT_STR(int);
DEFAULT_STR(Real);
DEFAULT_STR(String);
  

/*
  fucking C++ blows me.
 */

#define DEFAULT_ACCESSOR(Class, accessor)\
Class*\
Class ## _identifier::accessor () const {\
  ((Class ## _identifier*)this)->accessed_b_ = true;\
  return new Class (*data_p_);\
}

#define VIRTUAL_ACCESSOR(Class, accessor)\
Class*\
Class ## _identifier::accessor () const{\
  ((Class ## _identifier*)this)->accessed_b_ = true;\
  return (Class*)data_p_->clone();\
}

#define IMPLEMENT_ID_CLASS(Class, accessor)	\
	IMPLEMENT_IS_TYPE_B1(Class ## _identifier,Identifier)\
	Class ## _identifier::~Class ## _identifier() { delete data_p_; }\
	Class ## _identifier::Class ## _identifier (Class*st, int code):Identifier (code) { data_p_ = st; }\
Class ## _identifier::Class ## _identifier (Class ## _identifier const &s) \
: Identifier (s)\
{\
   data_p_ = s.accessor ();\
} 


IMPLEMENT_ID_CLASS(Duration, duration);
IMPLEMENT_ID_CLASS(Translator, translator);
IMPLEMENT_ID_CLASS(int, intid);
IMPLEMENT_ID_CLASS(Real, real);
IMPLEMENT_ID_CLASS(String, string);
IMPLEMENT_ID_CLASS(General_script_def, script);
IMPLEMENT_ID_CLASS(Lookup, lookup);
IMPLEMENT_ID_CLASS(Symtables, symtables);
IMPLEMENT_ID_CLASS(Music, music);
IMPLEMENT_ID_CLASS(Score, score);
IMPLEMENT_ID_CLASS(Request, request);
IMPLEMENT_ID_CLASS(Midi_def, mididef);
IMPLEMENT_ID_CLASS(Paper_def, paperdef);

VIRTUAL_ACCESSOR(Music, music);
VIRTUAL_ACCESSOR(Request, request);
VIRTUAL_ACCESSOR(Translator, translator);
VIRTUAL_ACCESSOR(General_script_def, script);

DEFAULT_ACCESSOR(Duration, duration);
DEFAULT_ACCESSOR(int, intid);
DEFAULT_ACCESSOR(Real, real);
DEFAULT_ACCESSOR(String, string);
DEFAULT_ACCESSOR(Lookup, lookup);
DEFAULT_ACCESSOR(Symtables, symtables);
DEFAULT_ACCESSOR(Score, score);
DEFAULT_ACCESSOR(Midi_def, mididef);
DEFAULT_ACCESSOR(Paper_def, paperdef);

