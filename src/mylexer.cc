#include "interval.hh"
#include "identparent.hh"
#include "associter.hh"
#include "lexer.hh"
#include "parser.hh"
#include "keyword.hh"
#include "assoc.hh"
#include "lexer.hh"
#include "sstack.hh"
#include "debug.hh"
#include "notename.hh"

static Keyword_ent the_key_tab[]={
    "bar", BAR,
    "bass", BASS,
    "cadenza", CADENZA,
    "clef", CLEF,
    "cm", CM,
    "commands", COMMANDS,
    "duration", DURATIONCOMMAND,
    "geometric", GEOMETRIC,
    "goto", GOTO,
    "in", IN,
    "key", KEY,
    "mark", MARK,
    "melodic", MELODIC,
    "meter", METER,
    "mm", MM,
    "octave", OCTAVECOMMAND,
    "output", OUTPUT,
    "partial", PARTIAL,
    "paper", PAPER,
    "plet", PLET,
    "pt", PT,
    "rhythmic", RHYTHMIC,
    "score", SCORE,
    "script", SCRIPT,
    "skip", SKIP,
    "staff", STAFF,
    "start", START_T,
    "table", TABLE,
    "symboltables", SYMBOLTABLES,
    "notenames", NOTENAMES,
    "texid", TEXID,
    "textstyle", TEXTSTYLE,
    "chord", CHORD,
    "multi", MULTI,
    "unitspace", UNITSPACE,
    "violin", VIOLIN,
    "voice", VOICE,
    "voices", VOICES,
    "width", WIDTH,
    "music", MUSIC,
    "grouping", GROUPING,
    0,0
};

My_flex_lexer::My_flex_lexer()
{
    keytable = new Keyword_table(the_key_tab);
    the_id_tab = new Assoc<String, Identifier*>;
    defaulttab = 0;
}

int
My_flex_lexer::lookup_keyword(String s)
{
    return keytable->lookup(s);
}

Identifier*
My_flex_lexer::lookup_identifier(String s)
{
    if (!the_id_tab->elt_query(s))
	return 0;
    
    return (*the_id_tab)[s];
}

void
My_flex_lexer::add_identifier(Identifier*i)
{
    delete lookup_identifier(i->name);
    (*the_id_tab)[i->name] = i;
}

My_flex_lexer::~My_flex_lexer()
{
    delete keytable;
    delete defaulttab;
    for (Assoc_iter<String,Identifier*> ai(*the_id_tab); ai.ok(); ai++) {
	mtor << "deleting: " << ai.key()<<'\n';
	delete ai.val();
    }
    delete the_id_tab;
}

String
My_flex_lexer::spot()const
{
    return include_stack.top()->name +  ": " + lineno();
}

void
My_flex_lexer::LexerError(const char *s)
{
    if (lexer->include_stack.empty()) {
	*mlog << "error at EOF" << s << '\n';
    }else 
	*mlog << spot() << ": error:" << s << '\n';
     exit(1);
}
// set the  new input to s, remember old file.
void
My_flex_lexer::new_input(String s)
{    
   if (!include_stack.empty())
	include_stack.top()->line = lineno();

   Input_file *newin = new Input_file(s);
   include_stack.push(newin);
   switch_streams(newin->is);
   yylineno = 1;
}

// pop the inputstack.
bool
My_flex_lexer::close_input()
{
    Input_file *old = include_stack.pop();
     bool ok = 	true;
    if (include_stack.empty()) {
	ok = false;
    } else {
	Input_file *i = include_stack.top();
	switch_streams(i->is);
	yylineno = i->line;	
    }
    delete old;
    return ok;
}
