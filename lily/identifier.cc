/*
  identifier.cc -- implement identifier and derived classes

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <assert.h>
#include "midi-def.hh"
#include "paper-def.hh"
#include "score.hh"
#include "identifier.hh"
#include "my-lily-lexer.hh"
#include "debug.hh"
#include "symtable.hh"
#include "script-def.hh"
#include "request.hh"
#include "translator.hh"
#include "notename-table.hh"

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
  e += _f ("%s expected", expect);
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
#define DEFAULT_PRINT(Class) \
void \
Class ## _identifier::do_print () const { \
  Class *cl = ((Class ## _identifier *)this)->access_ ## Class(false);\
  cl->print (); \
}



DEFAULT_PRINT(General_script_def);
DEFAULT_PRINT(Translator);
DEFAULT_PRINT(Symtables);
DEFAULT_PRINT(Music);
DEFAULT_PRINT(Request);
DEFAULT_PRINT(Score);
DEFAULT_PRINT(Midi_def);
DEFAULT_PRINT(Paper_def);

/* ugh. */
#define DUMMY_STR(Class) \
String \
Class ## _identifier::do_str () const { \
  return String (#Class); \
}


DUMMY_STR(Notename_table);
DUMMY_STR(General_script_def);
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
  DOUT << do_str () << '\n';\
}\


STRING_PRINT(Duration);
STRING_PRINT(Real);
STRING_PRINT(int);
STRING_PRINT(String);
STRING_PRINT(Notename_table);
  
#define DEFAULT_STR(Class) \
String \
Class ## _identifier::do_str () const\
{\
  return to_str (*data_p_);\
}

DEFAULT_STR(int);
DEFAULT_STR(Real);
DEFAULT_STR(String);
  

/*
  fucking C++ blows me.
 */

#define DEFAULT_ACCESSOR(Class)\
Class*\
Class ## _identifier::access_ ## Class (bool copy_b) const {\
  ((Class ## _identifier*)this)->accessed_b_ = true;\
  return copy_b ? new Class (*data_p_) : data_p_;\
}

#define VIRTUAL_ACCESSOR(Class)\
Class*\
Class ## _identifier::access_ ## Class (bool copy_b) const{\
  ((Class ## _identifier*)this)->accessed_b_ = true;\
  return copy_b ? (Class*)data_p_->clone() : data_p_;\
}

#define IMPLEMENT_ID_CLASS(Class)	\
	IMPLEMENT_IS_TYPE_B1(Class ## _identifier,Identifier)\
	Class ## _identifier::~Class ## _identifier() { delete data_p_; }\
	Class ## _identifier::Class ## _identifier (Class*st, int code) \
	  :Identifier (code)\
	{\
	  data_p_ = st;\
	}\
Class ## _identifier::Class ## _identifier (Class ## _identifier const &s) \
  : Identifier (s)\
{\
   data_p_ = s.access_ ## Class (true);\
} 


IMPLEMENT_ID_CLASS(Duration);
IMPLEMENT_ID_CLASS(Translator);
IMPLEMENT_ID_CLASS(int);
IMPLEMENT_ID_CLASS(Real);
IMPLEMENT_ID_CLASS(String);
IMPLEMENT_ID_CLASS(General_script_def);
IMPLEMENT_ID_CLASS(Symtables);
IMPLEMENT_ID_CLASS(Music);
IMPLEMENT_ID_CLASS(Score);
IMPLEMENT_ID_CLASS(Request);
IMPLEMENT_ID_CLASS(Midi_def);
IMPLEMENT_ID_CLASS(Paper_def);
IMPLEMENT_ID_CLASS(Notename_table);
VIRTUAL_ACCESSOR(Music);
VIRTUAL_ACCESSOR(Request);
VIRTUAL_ACCESSOR(Translator);
VIRTUAL_ACCESSOR(General_script_def);
DEFAULT_ACCESSOR(Notename_table);
DEFAULT_ACCESSOR(Duration);
DEFAULT_ACCESSOR(int);
DEFAULT_ACCESSOR(Real);
DEFAULT_ACCESSOR(String);
DEFAULT_ACCESSOR(Symtables);
DEFAULT_ACCESSOR(Score);
DEFAULT_ACCESSOR(Midi_def);
DEFAULT_ACCESSOR(Paper_def);

