/*
  identifier.cc -- implement identifier and derived classes

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <assert.h>

#include "identifier.hh"
#include "my-lily-lexer.hh"
#include "debug.hh"
#include "input-register.hh"
#include "input-score.hh" 
#include "symtable.hh"
#include "input-staff.hh"
#include "input-music.hh"
#include "lookup.hh"
#include "script-def.hh"
#include "request.hh"
#include "input-register.hh"

Identifier::~Identifier()
{
    if (!accessed_b_ && !init_b_)
	warning("Variable not used");
}
void
Identifier::error(String expect)
{
    String e("Wrong identifier type: ");
    e += String(classname()) + "(expected " + expect + ")";
    ::error(e);
}

Identifier::Identifier(String n, int code)
    :  name_str_(n) 
{
    token_code_i_ = code; 
    data = 0;
    accessed_b_ = 0;
    init_b_ = 0;
}

void
Identifier::print()const
{
    mtor << "identifier \'" << name_str_ << "\'=";
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
DEFAULT_PRINT(Score_id, Input_score, score);
DEFAULT_PRINT(Input_regs_id, Input_register, iregs);

void
Real_id::do_print() const
{
    Identifier::print();
    mtor << *((Real_id*)this)->real(false)<< "\n";
}

#define implement_id_class(Idclass, Class, accessor)	\
char const * Idclass::classname() const\
{\
    return #Class;\
}\
Class*\
Idclass::accessor(bool copy) {\
	if (copy){ \
	    accessed_b_ = true;\
	    return new Class(* (Class*) data);\
        }else\
	    return (Class*) data;\
    }\
Idclass::~Idclass() { delete accessor(false); }\
Idclass::Idclass(String s, Class*st, int code):Identifier(s,code) { data = st; }\


implement_id_class(Real_id, Real, real);
implement_id_class(Script_id, Script_def, script);
implement_id_class(Lookup_id, Lookup, lookup);
implement_id_class(Symtables_id, Symtables, symtables);
implement_id_class(Staff_id, Input_staff, staff);
implement_id_class(M_chord_id, Music_general_chord, mchord);
implement_id_class(M_voice_id, Music_voice, mvoice);
implement_id_class(Score_id, Input_score, score);
implement_id_class(Request_id, Request, request);
implement_id_class(Input_regs_id, Input_register, iregs);

Identifier::Identifier(Identifier const&)
{
    assert(false);
}
