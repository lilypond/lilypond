/*
  identifier.cc -- implement identifier and derived classes

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <assert.h>

#include "score.hh"
#include "identifier.hh"
#include "my-lily-lexer.hh"
#include "debug.hh"
#include "input-register.hh"
#include "symtable.hh"
#include "lookup.hh"
#include "script-def.hh"
#include "request.hh"
#include "input-register.hh"

IMPLEMENT_STATIC_NAME(Identifier);
IMPLEMENT_IS_TYPE_B(Identifier);

Identifier::~Identifier()
{
    if (!accessed_b_ && !init_b_)
	warning("Variable not used");
}
void
Identifier::error(String expect)
{
    String e("Wrong identifier type: ");
    e += String(name()) + "(expected " + expect + ")";
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

DEFAULT_PRINT(Script_id, General_script_def, script);
DEFAULT_PRINT(Lookup_id, Lookup, lookup);
DEFAULT_PRINT(Symtables_id, Symtables, symtables);
DEFAULT_PRINT(Music_id,Music , music);
DEFAULT_PRINT(Request_id, Request, request);
DEFAULT_PRINT(Score_id, Score, score);
DEFAULT_PRINT(Input_regs_id, Input_register, iregs);

void
Real_id::do_print() const
{
    Identifier::print();
    mtor << *((Real_id*)this)->real(false)<< "\n";
}

#define default_accessor(Idclass, Class, accessor)\
Class*\
Idclass::accessor(bool copy) {\
	if (copy){ \
	    accessed_b_ = true;\
	    return new Class(* (Class*) data);\
        }else\
	    return (Class*) data;\
    }\

#define virtual_accessor(Idclass, Class, accessor)\
Class*\
Idclass::accessor(bool copy) {\
	if (copy){ \
	    accessed_b_ = true;\
	    return (Class*) ((Class*) data)->clone();\
        }else\
	    return (Class*) data;\
    }\


#define implement_id_class(Idclass, Class, accessor)	\
IMPLEMENT_STATIC_NAME(Idclass)\
IMPLEMENT_IS_TYPE_B1(Idclass,Identifier)\
Idclass::~Idclass() { delete accessor(false); }\
Idclass::Idclass(String s, Class*st, int code):Identifier(s,code) { data = st; }\



implement_id_class(Real_id, Real, real);
implement_id_class(Script_id, General_script_def, script);
implement_id_class(Lookup_id, Lookup, lookup);
implement_id_class(Symtables_id, Symtables, symtables);
implement_id_class(Music_id, Music, music);
implement_id_class(Score_id, Score, score);
implement_id_class(Request_id, Request, request);
implement_id_class(Input_regs_id, Input_register, iregs);

Identifier::Identifier(Identifier const&)
{
    assert(false);
}


default_accessor(Real_id, Real, real);
virtual_accessor(Script_id, General_script_def, script);
default_accessor(Lookup_id, Lookup, lookup);
default_accessor(Symtables_id, Symtables, symtables);
virtual_accessor(Music_id, Music, music);
default_accessor(Score_id, Score, score);
virtual_accessor(Request_id, Request, request);
default_accessor(Input_regs_id, Input_register, iregs);
