#include <assert.h>

#include "identparent.hh"
#include "lexer.hh"
#include "debug.hh"

void
Identifier::error(String expect)
{
    String e("Wrong identifier type: ");
    e += String(classname()) + "(expected " + expect + ")";
    ::error(e);
}
