/*
  identifier.cc -- implement identifier and derived classes

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <assert.h>

#include "identifier.hh"
#include "lexer.hh"
#include "debug.hh"

void
Identifier::error(String expect)
{
    String e("Wrong identifier type: ");
    e += String(classname()) + "(expected " + expect + ")";
    ::error(e);
}

void
Identifier::print()const
{
    mtor << "identifier \'" << name << "\'=";
    do_print();
}

/* ugh. */
#define DEFAULT_PRINT(Class, Content_type, accessor) \
void \
Class::do_print() const { \
    ((Class*)this)->accessor(false)->print(); \
} \
class Class

DEFAULT_PRINT(Script_id, Script_def, script);
DEFAULT_PRINT(Lookup_id, Lookup, lookup);
DEFAULT_PRINT(Symtables_id, Symtables, symtables);
DEFAULT_PRINT(Staff_id, Input_staff, staff);
DEFAULT_PRINT(M_chord_id, Music_general_chord, mchord);
DEFAULT_PRINT(M_voice_id, Music_voice, mvoice);
DEFAULT_PRINT(Request_id, Request, request);
void
Real_id::do_print() const
{
    Identifier::print();
    mtor << *((Real_id*)this)->real(false)<< "\n";
}

