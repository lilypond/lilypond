#include <assert.h>

#include "identparent.hh"
#include "lexer.hh"
#include "debug.hh"

void
Identifier::error()
{
    String e("Wrong identifier type: ");
    ::error(e + classname());
}
