%{ // -*-Fundamental-*-
#include <iostream.h>

#include "lookup.hh"
#include "misc.hh"
#include "lexer.hh"
#include "paper.hh"
#include "inputscore.hh"
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

Array<Request*> pre_reqs, post_reqs;
Array<const char *> define_spots;
Paperdef*default_paper();
char const* defined_ch_c_l;
char const* req_defined_ch_c_l;
int fatal_error_i = 0;

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
    Input_music *music;
    Music_general_chord *chord;
    Music_voice *mvoice; 
    int i;
    char c;
    int ii[10];
	Moment *moment;

    Array<String> * strvec;
    Array<Input_command*> *commandvec;
    Array<int> *intvec;

    Input_staff *staff;    
    Input_score *score;
    Symtables * symtables;
    Symtable * symtable;
    Symbol * symbol;
    Lookup*lookup;
    Interval *interval;
    Box *box;
    Notename_tab *notename_tab;
    Script_def * script;
    Text_def * textdef;
}

%token VOICE STAFF SCORE TITLE  BAR  OUTPUT MULTIVOICE
%token CM_T IN_T PT_T MM_T PAPER WIDTH METER UNITSPACE SKIP COMMANDS COMMAND
%token GEOMETRIC START_T DURATIONCOMMAND OCTAVECOMMAND
%token KEY CLEF  TABLE  VOICES STEM
%token PARTIAL MUSIC GROUPING CADENZA
%token END SYMBOLTABLES TEXID TABLE NOTENAMES SCRIPT TEXTSTYLE PLET
%token MARK GOTO

%token <id>  IDENTIFIER
%token <string> PITCHMOD DURATION RESTNAME
%token <ii> NOTENAME 
%token <real> REAL 
%token <string> STRING

%token <i> DOTS INT
%type <real> unit
%type <intvec> pitch_list 
%type <c> open_request_parens close_request_parens
%type <id> declaration
%type <string> declarable_identifier
%type <paper> paper_block paper_body
%type <real> dim real
%type <ii>  default_duration explicit_duration notemode_duration mudela_duration
%type <ii> notename
%type <moment> duration_length
%type <el> voice_elt full_element lyrics_elt
%type <command> score_command staff_command position_command
%type <score> score_block score_body
%type <staff> staff_block staff_init staff_body
%type <i> int
%type <intvec> int_list intastint_list
%type <commandvec> score_commands_block score_commands_body
%type <commandvec> staff_commands_block staff_commands_body
%type <request> post_request pre_request 
%type <string> pitchmod
%type <music> music 
%type <chord> music_chord music_chord_body

%type <mvoice>  music_voice_body music_voice 

%type <interval> dinterval
%type <box> box
%type <symtable> symtable symtable_body
%type <lookup> symtables symtables_body
%type <symbol> symboldef
%type <notename_tab> notename_tab notename_tab_body
%type <i> script_dir
%type <script> script_definition script_body mudela_script
%type <request> script_req textscript_req
%type <textdef> mudela_text


%%

mudela:	/* empty */
	| mudela score_block {
		add_score($2);		
	}
	| mudela add_declaration { }
	| mudela mudela_command  {}
	;

mudela_command:
	notename_tab			{ lexer->set($1); }
	;

/*
	DECLARATIONS
*/
add_declaration: declaration	{
		lexer->add_identifier($1);
	}
	;

declarable_identifier:
	STRING { $$ = $1; }
	| IDENTIFIER { $$ = new String($1->name); }
	;

declaration:
	declarable_identifier '=' staff_block  {
		$$ = new Staff_id(*$1, $3);
		delete $1; // this sux
	}
	| declarable_identifier '=' music_voice {
		$$ = new M_voice_id(*$1, $3);
		delete $1;
	}
	| declarable_identifier '=' script_definition {
		$$ = new Script_id(*$1, $3);
		delete $1;
	}
	| declarable_identifier '=' music_chord  {
		$$ = new M_chord_id(*$1, $3);
		delete $1;
	}
	| declarable_identifier '=' symtables {
		$$ = new Lookup_id(*$1, $3);
		delete $1;
	}
	| declarable_identifier '=' notename_tab {
		$$ = new Notetab_id(*$1, $3);
		delete $1;
	}
	| declarable_identifier '=' real	{
		$$ = new Real_id(*$1, new Real($3));
		delete $1;
	}
	| declarable_identifier error '}' {
//		warning( "parse error", lexer->here_ch_c_l() );
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
score_block: SCORE 
		{ define_spots.push(lexer->here_ch_c_l()); }
	'{' score_body '}' 	{
		$$ = $4;
		$$->defined_ch_c_l_ = define_spots.pop();
		if (!$$->paper_)
			$$->paper_ = default_paper();

		/* handle error levels. */
		$$->errorlevel_i_ = lexer->errorlevel_i_;
		lexer->errorlevel_i_ = 0;
	}
	;

score_body:		{ 
		$$ = new Input_score; 
	}
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
	| COMMANDS '{' error '}' {
//		warning( "parse error", lexer->here_ch_c_l() );
	}
	;

score_commands_body:			{ $$ = new Array<Input_command*>; }
	| score_commands_body score_command		{
		$$->push($2);
	}
	| score_commands_body position_command		{
		$$->push($2);
	}
	;

staff_commands_block: COMMANDS '{' staff_commands_body '}'	{	
		$$ = $3; }
	;

staff_commands_body:
	/* empty */			{ $$ = new Array<Input_command*>; }
	| staff_commands_body staff_command	{
		$$->push($2);
	}
	| staff_commands_body position_command	{
		$$->push($2);
	}
	;

staff_command:
	KEY pitch_list 	{/*UGH*/
		$$ = get_key_interpret_command(*$2);
		delete $2;
	}
	| CLEF STRING			{
		$$ = get_clef_interpret_command(*$2);
		delete $2;
	}
	;

position_command:
	SKIP int ':' duration_length		{
		$$ = get_skip_command($2, *$4);
		delete $4;
	}
	| GOTO STRING	{
		$$ = get_goto_command(*$2);
		delete $2;
	}
	;


intastint_list:
	/* */	{ $$ =new Array<int>; }
	| intastint_list int '*' int	{
		$$->push($2); $$->push($4);
	}
	;

score_command:
	BAR STRING			{
		$$ = get_bar_command(*$2);
		delete $2;
	}
	| METER  int '*' int		{
		$$ = get_meterchange_command($2, $4);
	}
	| PARTIAL duration_length		{
		$$ = get_partial_command(*$2);
		delete $2;
	}
	| GROUPING intastint_list		{
		$$ = get_grouping_command(*$2);
		delete $2;
	}
	| CADENZA int	{
		$$ = get_cadenza_toggle($2);
	}
	;



/*
	PAPER
*/
paper_block:
	PAPER

	'{' paper_body '}' 	{ $$ = $3; }
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
	| paper_body error {
//		warning( "parse error", lexer->here_ch_c_l() );
	}
	;

/*
	STAFFs
*/
staff_block:
	STAFF 	{ define_spots.push(lexer->here_ch_c_l()); }
/*cont*/	'{' staff_body '}' 	{
		$$ = $4; 
		$$-> defined_ch_c_l_ = define_spots.pop();
	}
	;



staff_init:
	IDENTIFIER		{ $$ = $1->staff(true); }
	| STRING		{
		$$ = new Input_staff(*$1);
		delete $1;
	}
	;

staff_body:
	staff_init
	| staff_body music	{
		$2->set_default_group( "staff_music" + String($$->music_.size()));
		$$->add($2);
	}
	| staff_body staff_commands_block {
		$$->add(*$2);
		delete $2;
	}
	| staff_body error {
//		warning( "parse error", lexer->here_ch_c_l() );
	}
	;

/*
	MUSIC
*/
music:
	music_voice	{ $$ = $1; }
	| music_chord	{ $$ = $1; }
	;

music_voice:  MUSIC '{' music_voice_body '}'	{ $$ = $3; }
	;

music_voice_body:
	/* */ 	{
		$$ = new Music_voice;
	}
	| music_voice_body IDENTIFIER {
		$$->concatenate($2->mvoice(true));
	}
	| music_voice_body full_element {
		$$->add_elt($2);
	}
	| music_voice_body voice_command {
	}
	| music_voice_body music	{
		$$->add($2);
	}
	| music_voice_body error {
//		warning( "parse error", lexer->here_ch_c_l() );
	}
	;

music_chord:  '{' music_chord_body '}'	{ $$ = $2; }
	;

music_chord_body:
	/* */	{
		$$ = new Voice_group_chord;
	}
	| MULTIVOICE {
		$$ = new Multi_voice_chord;
	}
	| music_chord_body IDENTIFIER {
		$$->concatenate($2->mchord(true));
	}
	| music_chord_body music {
		$$ -> add($2);
	}
	| music_chord_body full_element {
		$$ ->add_elt($2);
	}
	| music_chord_body error {
//		warning( "parse error", lexer->here_ch_c_l() );
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
	| MARK STRING	{
		$$ = get_mark_element(*$2);
		delete $2;
	}
	| COMMAND '{' staff_command '}'	{ $$=get_command_element($3); }
	| COMMAND '{' score_command '}'	{ $$=get_command_element($3); }
	| '|'				{ 
		req_defined_ch_c_l = lexer->here_ch_c_l();
		$$ = get_barcheck_element(); 
	}
	| STEM '{' int '}'		{
		req_defined_ch_c_l = lexer->here_ch_c_l();
		$$ = get_stemdir_element($3);
	}
	| lyrics_elt
/*
+ 	| pre_requests voice_elt post_requests error '|' { 
+ 		warning( "parse error", lexer->here_ch_c_l() );
+ 	}
+ */
	;
		
post_requests:
	{
		assert(post_reqs.empty());
	}
	| post_requests post_request {
		post_reqs.push($2);
	}
	;

post_request:
	close_request_parens	{ 
		req_defined_ch_c_l = lexer->here_ch_c_l();
		$$ = get_request($1); 
	}
	| script_req
	| textscript_req
	;

close_request_parens:
	'('	{ 
		req_defined_ch_c_l = lexer->here_ch_c_l();
		$$='(';
	}
	| ']'	{ 
		req_defined_ch_c_l = lexer->here_ch_c_l();
		$$ = ']';
	}
	;
  
open_request_parens:
	')'	{ 
		req_defined_ch_c_l = lexer->here_ch_c_l();
		$$=')';
	}
	| '['	{
		req_defined_ch_c_l = lexer->here_ch_c_l();
		$$='[';
	}
	;

script_definition:
	SCRIPT '{' script_body '}' 	{ $$ = $3; }
	;

script_body:
	STRING int int int 		{
		$$ = new Script_def(*$1,$2, $3,$4);
		delete $1;
	}	
	;

textscript_req:
	script_dir mudela_text		{ $$ = get_text_req($1,$2); }
	;

mudela_text:
	STRING			{ 
		defined_ch_c_l = lexer->here_ch_c_l();
		$$ = get_text(*$1); 
		delete $1;
	}
	;

script_req:
	script_dir mudela_script	{ 
		$$ = get_script_req($1, $2);
	}
	;

mudela_script:
	IDENTIFIER		{ $$ = $1->script(true); }
	| script_definition		{ $$ = $1; }
	| '^'		{ $$ = get_scriptdef('^'); }
	| '+'		{ $$ = get_scriptdef('+'); }
	| '-'		{ $$ = get_scriptdef('-'); }
 	| '|'		{ $$ = get_scriptdef('|'); }
	| 'o'		{ $$ = get_scriptdef('o'); }
	| '>'		{ $$ = get_scriptdef('>'); }
	| '.' 		{ $$ = get_scriptdef('.'); }
	| DOTS 		{
		if ( $1 > 1 ) 
		    warning( "too many staccato dots", lexer->here_ch_c_l() );
		$$ = get_scriptdef('.');
	}
	| error {
		$$ = get_scriptdef('.');
		yyerrok;
	}
	;

script_dir:
	'_'	{ $$ = -1; }
	|'^'	{ $$ = 1; }
	|'-'	{ $$ = 0; }
	;

pre_requests:
	| pre_requests pre_request {
		pre_reqs.push($2);
	}
	;

pre_request: 
	open_request_parens	{ 
		defined_ch_c_l = lexer->here_ch_c_l();
		$$ = get_request($1); 
	}
	;

voice_command:
	PLET	'{' INT '/' INT '}'		{
		set_plet($3,$5);
	}
	| DURATIONCOMMAND '{' STRING '}'	{
		set_duration_mode(*$3);
		delete $3;
	}
	| DURATIONCOMMAND '{' notemode_duration '}'	{
		set_default_duration($3);
	}
	| OCTAVECOMMAND '{' pitchmod '}'	{
		set_default_octave(*$3);
		delete $3;
	}
	| TEXTSTYLE STRING	{
		set_text_style(*$2);
		delete $2;
	}
	;

duration_length:	
	mudela_duration		{
		$$ = new Moment(wholes($1[0], $1[1]));
	}
	|int '*' mudela_duration	{
		$$ = new Moment(Rational($1) * wholes($3[0], $3[1]));
	}
	;

notemode_duration:
	explicit_duration
	| default_duration
	;

mudela_duration:
	int		{
		$$[0] = $1;
		$$[1] = 0;
	}
	| int DOTS 	{
		$$[0] = $1;
		$$[1] = $2;
	}
	;


explicit_duration:
	INT		{
		last_duration($1);
		$$[0] = $1;
		$$[1] = 0;
	}
	| INT DOTS 	{
		last_duration($1);
		$$[0] = $1;
		$$[1] = $2;
	}
	| DOTS  {
                get_default_duration($$);
                $$[1] = $1;
	}
	;

default_duration:
	{
		get_default_duration($$);
	}
	;

pitchmod:		{ 
		defined_ch_c_l = lexer->here_ch_c_l();
		$$ = new String; 
	}
	| PITCHMOD	{ 
		defined_ch_c_l = lexer->here_ch_c_l();
		$$ = $1;
	}
	;

notename:
	NOTENAME
	;

voice_elt:
	pitchmod notename notemode_duration 			{
		$$ = get_note_element(*$1, $2, $3);
		delete $1;
	}
	| RESTNAME notemode_duration		{
		$$ = get_rest_element(*$1, $2);
		delete $1;

	}
	;

lyrics_elt:
	mudela_text notemode_duration 			{
		$$ = get_word_element($1, $2);
	};

/*
	UTILITIES
*/
pitch_list:			{
		$$ = new Array<int>;
	}
	| pitch_list NOTENAME	{
		$$->push($2[0]);
		$$->push($2[1]);		
	}
	;

int:
	real			{
		$$ = int($1);
		if ( distance($1,Real(int($$)) ) > 1e-8)
			error( "integer expected", lexer->here_ch_c_l() );
	}
	;

real:
	INT			{
		$$ = Real($1);
	}
	| REAL		{
		$$ = $1;
	}
	| IDENTIFIER		{
		$$ = * $1->real(0);		
	}
	;
	

int_list:		{
		$$ = new Array<int>;
	}
	| int_list int		{
		$$->push($2);
	}
	;


dim:
	real unit	{ $$ = $1*$2; }
	;


unit:	CM_T		{ $$ = 1 CM; }
	|IN_T		{ $$ = 1 INCH; }
	|MM_T		{ $$ = 1 MM; }
	|PT_T		{ $$ = 1 PT; }
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
	STRING 	box		{
		$$ = new Symbol(*$1, *$2);
		delete $1;
		delete $2;
	}
	| STRING {
		Box b;
		$$ = new Symbol(*$1, b);
		delete $1;
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
yyerror(const char *s)
{
//	if ( YYRECOVERING() ) 
//		return;
	lexer->LexerError(s);

	if ( fatal_error_i )
		exit( fatal_error_i );
}

void
parse_file(String init, String s)
{
   *mlog << "Parsing ... ";
   lexer = new My_flex_lexer;

#ifdef YYDEBUG
   yydebug = !monitor.silence("InitParser") && check_debug;
   lexer->set_debug( !monitor.silence("InitLexer") && check_debug);
#endif

   lexer->new_input(init);
   yyparse();

#ifdef YYDEBUG
   yydebug = !monitor.silence("Parser") && check_debug;
   lexer->set_debug( !monitor.silence("Lexer") && check_debug);
#endif

   lexer->new_input(s);
   yyparse();
   delete lexer;
   lexer = 0;

   assert(define_spots.empty());
}

Paperdef*
default_paper()
{
    return new Paperdef(
	lexer->lookup_identifier("default_table")->lookup(true));
}


