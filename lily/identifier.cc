/*
  identifier.cc -- implement identifier and derived classes

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
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

Identifier::~Identifier()
{
  if (!accessed_b_ && !init_b_)
	warning (_("Variable not used"));
}
void
Identifier::error (String expect)
{
  String e (_("Wrong identifier type: "));
  e += String (name()) + _("(expected ") + expect + ")";
  ::error (e);
}

Identifier::Identifier (int code)
{
  token_code_i_ = code;
  accessed_b_ = 0;
  init_b_ = 0;
}

void
Identifier::print() const
{
  DOUT << "identifier ";
  do_print();
}

/* ugh. */
#define DEFAULT_PRINT(Class, Content_type, accessor) \
void \
Class::do_print() const { \
  ((Class*)this)->accessor()->print(); \
} \
class Class


DEFAULT_PRINT(Script_id, General_script_def, script);
DEFAULT_PRINT(Lookup_id, Lookup, lookup);
DEFAULT_PRINT(Translator_id, Translator, translator);
DEFAULT_PRINT(Symtables_id, Symtables, symtables);
DEFAULT_PRINT(Music_id,Music , music);
DEFAULT_PRINT(Request_id, Request, request);
DEFAULT_PRINT(Score_id, Score, score);
DEFAULT_PRINT(Midi_def_id,Midi_def, mididef);
DEFAULT_PRINT(Paper_def_id,Paper_def, paperdef);


void
Duration_id::do_print() const
{}

void
Real_id::do_print() const
{
  DOUT << *data_p_<< "\n";
}

void
Int_id::do_print() const
{
  DOUT << *data_p_<< "\n";
}


#define DEFAULT_ACCESSOR(Idclass, Class, accessor)\
Class*\
Idclass::accessor () {\
  accessed_b_ = true;\
  return new Class (*data_p_);\
}

#define VIRTUAL_ACCESSOR(Idclass, Class, accessor)\
Class*\
Idclass::accessor () {\
  accessed_b_ = true;\
  return (Class*)data_p_->clone();\
}

#define IMPLEMENT_ID_CLASS(Idclass, Class, accessor)	\
	IMPLEMENT_IS_TYPE_B1(Idclass,Identifier)\
	Idclass::~Idclass() { delete data_p_; }\
	Idclass::Idclass (Class*st, int code):Identifier (code) { data_p_ = st; }\

IMPLEMENT_ID_CLASS(Duration_id, Duration, duration);
IMPLEMENT_ID_CLASS(Translator_id, Translator, translator);
IMPLEMENT_ID_CLASS(Int_id, int, intid);
IMPLEMENT_ID_CLASS(Real_id, Real, real);
IMPLEMENT_ID_CLASS(Script_id, General_script_def, script);
IMPLEMENT_ID_CLASS(Lookup_id, Lookup, lookup);
IMPLEMENT_ID_CLASS(Symtables_id, Symtables, symtables);
IMPLEMENT_ID_CLASS(Music_id, Music, music);
IMPLEMENT_ID_CLASS(Score_id, Score, score);
IMPLEMENT_ID_CLASS(Request_id, Request, request);
IMPLEMENT_ID_CLASS(Midi_def_id, Midi_def, mididef);
IMPLEMENT_ID_CLASS(Paper_def_id, Paper_def, paperdef);

Identifier::Identifier (Identifier const&s)
  : Input (s)
{
  assert (false);
}

DEFAULT_ACCESSOR(Duration_id, Duration, duration);
VIRTUAL_ACCESSOR(Translator_id,Translator, translator);
DEFAULT_ACCESSOR(Int_id, int, intid);
DEFAULT_ACCESSOR(Real_id, Real, real);
VIRTUAL_ACCESSOR(Script_id, General_script_def, script);
DEFAULT_ACCESSOR(Lookup_id, Lookup, lookup);
DEFAULT_ACCESSOR(Symtables_id, Symtables, symtables);
VIRTUAL_ACCESSOR(Music_id, Music, music);
DEFAULT_ACCESSOR(Score_id, Score, score);
VIRTUAL_ACCESSOR(Request_id, Request, request);
DEFAULT_ACCESSOR(Midi_def_id, Midi_def, mididef);
DEFAULT_ACCESSOR(Paper_def_id, Paper_def, paperdef);
