#include <assert.h>

#include "identparent.hh"
#include "lexer.hh"

void
Identifier::error()
{
    String e("Wrong identifier type: ");
    yyerror(e + classname());
}
