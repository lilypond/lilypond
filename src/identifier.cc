#include <assert.h>
#include "identifier.hh"
#include "staff.hh"
#include "lexer.hh"


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

Voice_id::~Voice_id()
{
    delete voice();
}
