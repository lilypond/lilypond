#include <assert.h>
#include "identifier.hh"
#include "staff.hh"
#include "lexer.hh"
#include "inputmusic.hh"


Identifier::Identifier(String n)
    :name  (n)
{
    data = 0;
}


Identifier::~Identifier()
{    
}

Staff_id::~Staff_id()
{
    delete staff();
}

Voices_id::~Voices_id()
{
    voices()->junk();
    delete voices();
}
