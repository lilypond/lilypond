%{ // -*-Fundamental-*-
#include <iostream.h>

#include "lookup.hh"
#include "lexer.hh"
#include "paper.hh"
#include "inputstaff.hh"
#include "inputscore.hh"
#include "inputmusic.hh"
#include "main.hh"
#include "keyword.hh"
#include "inputcommand.hh"
#include "debug.hh"
#include "parseconstruct.hh"
#include "dimen.hh"
#include "identifier.hh"

#ifndef NDEBUG
#define YYDEBUG 1
#endif

svec<Request*> pre_reqs, post_reqs;

Paperdef*default_paper();
%}


%union {
    Request * request;
    Real real;
    Input_command *command;
    Identifier *id;    
    Voice *voice;    
    Voice_element *el;	
    String *string;
    const char *consstr;
    Paperdef *paper;
    Horizontal_music *horizontal;
    Vertical_music *vertical;
    Music_general_chord *chord;
    Music_voice *mvoice; 
    int i;
    char c;
    int ii[10];

    svec<String> * strvec;
    svec<Input_command*> *commandvec;
    svec<int> *intvec;

    Input_staff *staff;    
    Input_score *score;
    Symtables * symtables;
    Symtable * symtable;
    Symbol * symbol;
    Lookup*lookup;
    Interval *interval;
    Box *box;
    Notename_tab *notename_tab;
}

%token VOICE STAFF SCORE TITLE  BAR NOTENAME OUTPUT
%token CM IN PT MM PAPER WIDTH METER UNITSPACE SKIP COMMANDS
%token GEOMETRIC START_T DURATIONCOMMAND OCTAVECOMMAND
%token KEY CLEF VIOLIN BASS MULTI TABLE CHORD VOICES
%token PARTIAL RHYTHMIC MELODIC MUSIC GROUPING
%token END SYMBOLTABLES TEXID TABLE NOTENAMES

%token <id>  IDENTIFIER
%token <string> NEWIDENTIFIER 
%token <string> PITCHMOD DURATION RESTNAME
%token <ii> NOTENAME 
%token <real> REAL
%token <string> STRING
%token <i> OPEN_REQUEST_PARENS CLOSE_REQUEST_PARENS
%token <i> DOTS INT
%type <consstr> unit
%type <intvec> pitch_list

%type <id> declaration 
%type <paper> paper_block paper_body
%type <real> dim
%type <ii> duration
%type <el> voice_elt full_element
%type <command> score_command staff_command skipcommand
%type <score> score_block score_body
%type <staff> staff_block staff_init staff_body
%type <i> int
%type <intvec> int_list
%type <commandvec> score_commands_block score_commands_body
%type <commandvec> staff_commands_block staff_commands_body
%type <request> post_request pre_request
%type <string> clef_id pitchmod
%type <vertical> vertical_music  
%type <chord> music_chord music_chord_body
%type <horizontal>  horizontal_music
%type <mvoice>  music_voice_body music_voice

%type <interval> dinterval
%type <box> box
%type <symtable> symtable symtable_body
%type <lookup> symtables symtables_body
%type <symbol> symboldef
%type <notename_tab> notename_tab notename_tab_body

%%

mudela:	/* empty */
	| mudela score_block { 
		add_score($2);
	}
	| mudela add_declaration { }
	| mudela mudela_command  {}
	;

mudela_command:
	notename_tab			{ set_notename_tab($1); }
	;

/*
	DECLARATIONS
*/
add_declaration: declaration	{
		add_identifier($1);
	}
	;

declaration:
	NEWIDENTIFIER '=' staff_block  {
		$$ = new Staff_id(*$1, $3);
		delete $1; // this sux
	}
	| NEWIDENTIFIER '=' music_voice {
		$$ = new M_voice_id(*$1, $3);
		delete $1;
	}
	| NEWIDENTIFIER '=' music_chord  {
		$$ = new M_chord_id(*$1, $3);
		delete $1;
	}
	| NEWIDENTIFIER '=' symtables {
		$$ = new Lookup_id(*$1, $3);
		delete $1;
	}
	| NEWIDENTIFIER '=' notename_tab {
		$$ = new Notetab_id(*$1, $3);
		delete $1;
	}
	;

notename_tab:
	NOTENAMES '{' notename_tab_body '}'	{ $$ = $3; }
	;

notename_tab_body:				{
		$$ = new Notename_tab;
	}
	| IDENTIFIER				{
		$$ = $1->notename_tab(true);
	}
	| notename_tab_body STRING int int			{
		$$->set($3, $4, *$2);
		delete $2;
	}
	;

/*
	SCORE
*/
score_block: SCORE '{' score_body '}' 	{ $$ = $3;
		if (!$$->paper_)
			$$->paper_ = default_paper();
	}
	;

score_body:		{ $$ = new Input_score; }
	| score_body staff_block	{ $$->add($2); }
	| score_body score_commands_block 	{
		$$->add(*$2);
		delete $2;
	}
	| score_body paper_block		{ $$->set($2);	}
	;
/*
	COMMANDS
*/
score_commands_block:
	COMMANDS '{' score_commands_body '}' { $$ =$3;}
	;

score_commands_body:			{ $$ = new svec<Input_command*>; }
	| score_commands_body score_command		{
		$$->add($2);
	}
	;

staff_commands_block: COMMANDS '{' staff_commands_body '}'	{
		$$ = $3; }
	;

staff_commands_body:
	/* empty */			{ $$ = new svec<Input_command*>; }
	| staff_commands_body staff_command	{
		$$->add($2);
	}
	;

staff_command:
	skipcommand
	| KEY pitch_list 	{/*UGH*/
		$$ = get_key_interpret_command(*$2);
		delete $2;
	}
	| CLEF clef_id			{
		$$ = get_clef_interpret_command(*$2);
		delete $2;
	}
	;

skipcommand:
	SKIP int ':' REAL		{
		$$ = get_skip_command($2, $4);
	}

score_command:
	skipcommand
	| METER  int int		{
		$$ = get_meterchange_command($2, $3);
	}
	| PARTIAL REAL			{
		$$ = get_partial_command($2);
	}
	| GROUPING int_list		{
		$$ = get_grouping_command(*$2);
		delete $2;
	}
	;



/*
	PAPER
*/
paper_block:
	PAPER '{' paper_body '}' 	{ $$ = $3; }
	;

paper_body:
	/* empty */		 	{
		$$ = default_paper();
	}
	| paper_body WIDTH dim		{ $$->linewidth = $3;}
	| paper_body OUTPUT STRING	{ $$->outfile = *$3;
		delete $3;
	}
	| paper_body symtables		{ $$->set($2); }
	| paper_body UNITSPACE dim	{ $$->whole_width = $3; }
	| paper_body GEOMETRIC REAL	{ $$->geometric_ = $3; }
	;

/*
	STAFFs
*/
staff_block:
	STAFF '{' staff_body '}' 	{ $$ = $3; }
	;



staff_init:
	IDENTIFIER		{ $$ = $1->staff(true); }
	| RHYTHMIC		{
		$$ = new Input_staff("rhythmic");
	}
	| MELODIC		{
		$$ = new Input_staff( "melodic");
	}
	;

staff_body:
	staff_init
	| staff_body		horizontal_music	{
		$$->add($2);
	}
	| staff_body staff_commands_block {
		$$->add(*$2);
		delete $2;
	}
	;

/*
	MUSIC
*/
horizontal_music:
	music_voice	{ $$ = $1; }
	;

vertical_music:
	music_chord	{ $$ = $1; }
	;

music_voice: MUSIC '{' music_voice_body '}'	{ $$ = $3; }
	;

music_voice_body:			{
		$$ = new Music_voice;
	}
	| music_voice_body IDENTIFIER {
		$$->concatenate($2->mvoice());
	}
	| music_voice_body full_element {
		$$->add($2);
	}
	| music_voice_body voice_command {
	}
	| music_voice_body vertical_music	{
		$$->add($2);
	}
	;


music_chord: CHORD '{' music_chord_body '}'	{ $$ = $3; }
	;

music_chord_body:		{
		$$ = new Music_general_chord;
	}
	| music_voice_body IDENTIFIER {
		$$->concatenate($2->mchord());
	}
	| music_chord_body horizontal_music {
		$$ -> add($2);
	}
	;



/*
	VOICE ELEMENTS
*/
full_element:	pre_requests voice_elt post_requests {
		add_requests($2, pre_reqs);
		add_requests($2, post_reqs);
		$$ = $2;
	}
	;

post_requests:
	{
		assert(post_reqs.empty());
	}
	| post_requests post_request {
		post_reqs.add($2);
	}
	;

post_request:
	CLOSE_REQUEST_PARENS		{ $$ = get_request($1); }
	;

pre_requests:
	| pre_requests pre_request {
		pre_reqs.add($2);
	}
	;

pre_request: 
	OPEN_REQUEST_PARENS		{ $$ = get_request($1); }
	;



voice_command:
	DURATIONCOMMAND '{' duration '}'	{
		set_default_duration($3);
	}
	| OCTAVECOMMAND '{' pitchmod '}'	{
		set_default_octave(*$3);
		delete $3;
	}
	;

duration:		{
		get_default_duration($$);
	}
	| int		{
		get_default_duration($$);
		$$[0] = $1;
	}
	| int DOTS 	{
		$$[0] = $1;
		$$[1] = $2;
	}
	;

pitchmod:		{ $$ = new String; }
	|PITCHMOD	
	;

voice_elt:	
	pitchmod NOTENAME duration 			{
		$$ = get_note_element(*$1, $2, $3);
		delete $1;
	}
	| RESTNAME duration		{
		$$ = get_rest_element(*$1, $2);
		delete $1;

	}
	;

/*
	UTILITIES
*/
pitch_list:			{
		$$ = new svec<int>;
	}
	| pitch_list NOTENAME	{
		$$->add($2[0]);
		$$->add($2[1]);		
	}
	;

int:
	REAL			{
		$$ = int($1);
		if (ABS($1-Real(int($$))) > 1e-8)
			yyerror("expecting integer number");
	}
	| INT
	;

int_list:
	/* */ 		{
		$$ = new svec<int>;
	}
	| int		{
		$$->add($1);
	}
	;

dim:
	REAL unit	{ $$ = convert_dimen($1,$2); }
	;


unit:	CM		{ $$ = "cm"; }
	|IN		{ $$ = "in"; }
	|MM		{ $$ = "mm"; }
	|PT		{ $$ = "pt"; }
	;
	
clef_id:
	VIOLIN		{ $$ = new String("violin"); }
	| BASS		{ $$ = new String("bass"); }
	;
/*
	symbol tables
*/
symtables:
	SYMBOLTABLES '{' symtables_body '}'	{ $$ = $3; }
	;

symtables_body:
	 		{
		$$ = new Lookup;
	}
	| IDENTIFIER		{
		$$ = new Lookup(*$1->lookup(true));
	}
	| symtables_body TEXID STRING 		{
		$$->texsetting = *$3;
		delete $3;
	}
	| symtables_body STRING '=' symtable		{
		$$->add(*$2, $4);
		delete $2;
	}
	;

symtable:
	TABLE '{' symtable_body '}' { $$ = $3; }
	;

symtable_body:
				{ $$ = new Symtable; }
	| symtable_body	STRING	symboldef {
		$$->add(*$2, *$3);
		delete $2;
		delete $3;
	}
	;

symboldef:
	STRING	box		{
		$$ = new Symbol(*$1, *$2);
		delete $1;
		delete $2;
	}
	;

box:
	dinterval dinterval 	{
		$$ = new Box(*$1, *$2);
		delete $1;
		delete $2;
	}
	;

dinterval: dim	dim		{
		$$ = new Interval($1, $2);	
	}
	;

%%

void
parse_file(String s)
{
   *mlog << "Parsing ... ";

#ifdef YYDEBUG
   yydebug = !monitor.silence("Parser") && check_debug;
#endif

   new_input("symbol.ini");
   yyparse();
   new_input(s);
   yyparse();

   delete_identifiers();
   kill_lexer();
   *mlog << "\n";
}

Paperdef*
default_paper()
{
	return new Paperdef(lookup_identifier("default_table")->lookup(true));
}
