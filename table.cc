#include "glob.hh"
#include "debug.hh"
#include "string.hh"
#include "keyword.hh"
#include "parser.hh"

static Keyword_ent  the_key_tab[]={
    "voice", VOICE,
    "rhythmstaff", RHYTHMSTAFF,
    "score", SCORE,
    "bar", BAR,
    "output", OUTPUT,
    "cm", CM,
    "pt", PT,
    "in", IN,
    "mm", MM,
    "paper", PAPER,
    "width", WIDTH,
    "meter", METER,
    "unitspace", UNITSPACE,
    "skip", SKIP,
    "commands", COMMANDS,
    0,0
} ;


int
lookup_keyword(String s)
{
    static Keyword_table table(the_key_tab);
    return table.lookup(s);
}

Assoc<String, Identifier*> the_id_tab;

Identifier*
lookup_identifier(String s)
{
    if (!the_id_tab.elt_query(s))
	 the_id_tab[s]= new Identifier;
    
    return the_id_tab[s];
}
