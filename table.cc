#include "glob.hh"
#include "string.hh"
#include "keyword.hh"
#include "parser.hh"

static Keyword_ent  the_key_tab[]={
    "voice", VOICE,
    "rhythmstaff", RHYTHMSTAFF,
    "score", SCORE,
    "bar", BAR,
    0,0
} ;


int
lookup_keyword(String s)
{
    static Keyword_table table(the_key_tab);
    return table.lookup(s);
}

Identifier*
lookup_identifier(String s)
{
    assert(false);
    return 0;
}
