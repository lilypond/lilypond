#include "glob.hh"
#include "debug.hh"
#include "string.hh"
#include "identifier.hh"
#include "keyword.hh"
#include "associter.hh"
#include "parser.hh"

static Keyword_ent  the_key_tab[]={
    "voice", VOICE,
    "rhythmstaff", RHYTHMSTAFF,
    "melodicstaff", MELODICSTAFF,
    "score", SCORE,
    "bar", BAR,
    "output", OUTPUT,
    "cm", CM,
    "start", START_T,
    "pt", PT,
    "in", IN,
    "mm", MM,
    "paper", PAPER,
    "width", WIDTH,
    "meter", METER,
    "unitspace", UNITSPACE,
    "skip", SKIP,
    "octave", OCTAVECOMMAND,
    "commands", COMMANDS,
    "staff", STAFF,
    "geometric", GEOMETRIC,
    "duration", DURATIONCOMMAND,
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
	return 0;
    
    return the_id_tab[s];
}

void
add_identifier(Identifier*i)
{    
    the_id_tab[i->name] = i;
}

void
delete_identifiers()
{
    
    for (Assoc_iter<String,Identifier*> ai(the_id_tab); ai.ok(); ai++) {
	mtor << "deleting: " << ai.key()<<'\n';
	delete ai.val();
    }
}
