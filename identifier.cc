#include <assert.h>
#include "keyword.hh"
#include "lexer.hh"
#include "parser.hh"

Identifier::Identifier()
{
    data = 0;
    type = IDENTIFIER;
}


Identifier::~Identifier()
{
    if (!data)
	return;
    switch (type) {
    case IDENTIFIER:
    default:
	assert(false);
    }
}
