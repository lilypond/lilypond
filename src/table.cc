#include "glob.hh"
#include "debug.hh"
#include "string.hh"
#include "identifier.hh"
#include "keyword.hh"
#include "associter.hh"
#include "parser.hh"

static Keyword_ent the_key_tab[]={
    "bar", BAR,
    "bass", BASS,
    "clef", CLEF,
    "cm", CM,
    "commands", COMMANDS,
    "duration", DURATIONCOMMAND,
    "geometric", GEOMETRIC,
    "in", IN,
    "key", KEY, 
    "melodicstaff", MELODICSTAFF,
    "meter", METER,
    "mm", MM,
    "octave", OCTAVECOMMAND,
    "output", OUTPUT,
    "partial", PARTIAL,
    "paper", PAPER,
    "pt", PT,
    "rhythmstaff", RHYTHMSTAFF,
    "score", SCORE,
    "skip", SKIP,
    "staff", STAFF,
    "start", START_T,
    "table", TABLE,
    "chord", CHORD,
    "multi", MULTI,
    "unitspace", UNITSPACE,
    "violin", VIOLIN,
    "voice", VOICE,
    "voices", VOICES,
    "width", WIDTH,   
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
