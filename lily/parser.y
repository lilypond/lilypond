%{ // -*-Fundamental-*-
#include <iostream.h>
#include "script-def.hh"
#include "symtable.hh"
#include "lookup.hh"
#include "misc.hh"
#include "my-lily-lexer.hh"
#include "paper-def.hh"
#include "midi-def.hh"
#include "main.hh"
#include "input-score.hh"
#include "input-staff.hh"
#include "input-music.hh"
#include "keyword.hh"
#include "debug.hh"
#include "parseconstruct.hh"
#include "dimen.hh"
#include "identifier.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "voice-element.hh"

#ifndef NDEBUG
#define YYDEBUG 1
#endif

Array<Request*> pre_reqs, post_reqs;
Array<const char *> define_spots;
Paper_def*default_paper();
char const* defined_ch_c_l;
char const* req_defined_ch_c_l;
int fatal_error_i = 0;
bool init_parse_b;
%}


%union {
    Array<Melodic_req*> *melreqvec;
    Array<String> * strvec;
    Array<int> *intvec;
    Box *box;
    Identifier *id;    
    Input_music *music;
    Input_score *score;
    Input_staff *staff;    
    Interval *interval;
    Lookup*lookup;
    Melodic_req * melreq;
    Midi_def* midi;
    Moment *moment;
    Music_general_chord *chord;
    Music_voice *mvoice;
    Note_req *notereq;
    Paper_def *paper;
    Real real;
    Request * request;
    Script_def * script;
    String *string;
    Symbol * symbol;
    Symtable * symtable;
    Symtables * symtables;
    Text_def * textdef;
    Voice *voice;    
    Voice_element *el;	
    char c;
    const char *consstr;
    int i;
    int ii[10];
}

%token BAR
%token CADENZA
%token CLEF
%token CM_T
%token CONCAT
%token DURATIONCOMMAND
%token DYNAMIC
%token END
%token GEOMETRIC
%token GROUPING
%token IN_T
%token LYRICS
%token KEY
%token MELODIC
%token METER
%token MIDI
%token MM_T
%token MULTIVOICE
%token MUSIC
%token OCTAVECOMMAND
%token OUTPUT
%token PAPER
%token PARTIAL
%token PLET
%token PT_T
%token SCORE
%token SCRIPT
%token SKIP
%token STAFF
%token START_T
%token STEM
%token SYMBOLTABLES
%token TABLE
%token TRANSPOSE
%token TEMPO
%token TEXID
%token TEXTSTYLE
%token TITLE
%token UNITSPACE
%token WIDTH

%token <i>	DOTS
%token <i>	INT
%token <id>	IDENTIFIER
%token <id>	MELODIC_REQUEST_IDENTIFIER 
%token <id>	CHORD_IDENTIFIER
%token <id>	VOICE_IDENTIFIER
%token <id>	POST_REQUEST_IDENTIFIER
%token <id>	SCRIPT_IDENTIFIER
%token <id>	STAFF_IDENTIFIER
%token <id>	REAL_IDENTIFIER
%token <id>	SCORE_IDENTIFIER
%token <id>	REQUEST_IDENTIFIER
%token <real>	REAL 
%token <string>	DURATION RESTNAME
%token <string>	STRING

%type <box>	box
%type <c>	open_request_parens close_request_parens close_plet_parens
%type <chord>	music_chord music_chord_body  init_music_chord
%type <el>	voice_elt full_element lyrics_elt command_elt
%type <i>	int
%type <i>	octave_quotes octave_quote
%type <i>	script_dir
%type <id>	declaration
%type <ii>	default_duration explicit_duration notemode_duration
%type <ii>	mudela_duration
%type <interval>	dinterval
%type <intvec>	intastint_list
%type <lookup>	symtables symtables_body
%type <melreq>	melodic_request steno_melodic_req
%type <notereq>	steno_note_req
%type <melreqvec>	pitch_list 
%type <midi>	midi_block midi_body
%type <moment>	duration_length
%type <music>	init_music
%type <mvoice>	 transposed_music_voice init_lyrics_voice
%type <mvoice>	music_voice_body music_voice  init_music_voice 
%type <paper>	paper_block paper_body
%type <real>	dim real
%type <real>	unit
%type <request>	post_request pre_request command_req pure_post_request
%type <request>	script_req textscript_req dynamic_req 
%type <score>	score_block score_body
%type <script>	script_definition script_body mudela_script
%type <staff>	staff_block staff_init staff_body
%type <string>	declarable_identifier
%type <symbol>	symboldef
%type <symtable>	symtable symtable_body
%type <textdef>	mudela_text


%left PRIORITY

%expect 1	/* have to fix this. */

%%

mudela:	/* empty */
	| mudela score_block {
		add_score($2);		
	}
	| mudela add_declaration { }
	;


/*
	DECLARATIONS
*/
add_declaration: declaration	{
		lexer->add_identifier($1);
		$1->init_b_ = init_parse_b;
		$1->defined_ch_C_ = define_spots.pop();
	}
	;

declarable_identifier:
	STRING {
		define_spots.push(lexer->here_ch_c_l());
	    $$ = $1;
	    if (lexer->lookup_identifier(*$1))
		warning("redeclaration of `" + *$1 + "'",
			lexer->here_ch_c_l());
	}
	| IDENTIFIER { 
		define_spots.push(lexer->here_ch_c_l());
		$$ = new String($1->name); 
	}
	;

declaration:
	declarable_identifier '=' score_block {
		$$ = new Score_id(*$1, $3, SCORE_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' staff_block  {
		$$ = new Staff_id(*$1, $3, STAFF_IDENTIFIER);
		delete $1; 
	}
	| declarable_identifier '=' init_music_voice {
		$$ = new M_voice_id(*$1, $3, VOICE_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' init_lyrics_voice {
		$$ = new M_voice_id(*$1, $3, VOICE_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' script_definition {
		$$ = new Script_id(*$1, $3, SCRIPT_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' init_music_chord  {
		$$ = new M_chord_id(*$1, $3, CHORD_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' symtables {
		$$ = new Lookup_id(*$1, $3, IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' real	{
		$$ = new Real_id(*$1, new Real($3), REAL_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier error '}' {

	}
	| declarable_identifier '=' pure_post_request {
		$$ = new Request_id(*$1, $3, POST_REQUEST_IDENTIFIER);
		delete $1;
	}
	| declarable_identifier '=' melodic_request {
		$$ = new Request_id(*$1, $3, MELODIC_REQUEST_IDENTIFIER);
		delete $1;
	}
	;


/*
	SCORE
*/
score_block:
	SCORE { define_spots.push(lexer->here_ch_c_l()); }
	/*cont*/ '{' score_body '}' 	{
		$$ = $4;
		$$->defined_ch_c_l_ = define_spots.pop();
		if (!$$->paper_p_ && ! $$->midi_p_)
			$$->paper_p_ = default_paper();

		/* handle error levels. */
		$$->errorlevel_i_ = lexer->errorlevel_i_;
		lexer->errorlevel_i_ = 0;
	}
	;

score_body:		{ 
		$$ = new Input_score; 
	}
	| SCORE_IDENTIFIER {
		$$ = $1->score(true);
	}
	| score_body staff_block	{ $$->add($2); }
	| score_body paper_block		{ $$->set($2);	}
	| score_body midi_block		{ $$->set($2);	}
	| score_body error {

	}
	;

intastint_list:
	/* */	{ $$ =new Array<int>; }
	| intastint_list int '*' int	{
		$$->push($2); $$->push($4);
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

	}
	;

/*
	MIDI
*/
midi_block:
	MIDI

	'{' midi_body '}' 	{ $$ = $3; }
	;

midi_body: { 
		$$ = new Midi_def; 
	}
	| midi_body OUTPUT STRING	{ 
		$$->outfile_str_ = *$3; 
		delete $3; 
	}
	| midi_body TEMPO mudela_duration ':' int {
		$$->set_tempo( wholes( $3[0], $3[1] ), $5 );
	}
	| midi_body error {

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
	STAFF_IDENTIFIER		{ $$ = $1->staff(true); }
	| STRING		{
		$$ = new Input_staff(*$1);
		delete $1;
	}
	| MELODIC {
		$$ = new Input_staff("melodic");
	}
	;

staff_body:
	staff_init
	| staff_body init_music	{
		$2->set_default_group( "staff_music" + String($$->music_.size()));
		$$->add($2);
	}
	| staff_body error {
	}
	;

/*
	let the lexer switch mode.
*/
init_music:
	init_music_voice	{ $$ = $1; }
	| init_music_chord	{ $$ = $1; }
	| init_lyrics_voice	{ $$ = $1; }
	;

init_lyrics_voice:
	LYRICS { lexer->push_lyric_state(); } 
	music_voice { $$ = $3; lexer->pop_state(); }
	;

init_music_voice:
	MUSIC { lexer->push_note_state(); } 
	/* cont*/ music_voice
		{ $$=$3; lexer->pop_state(); }
	;
init_music_chord:
	MUSIC { lexer->push_note_state(); } 
	/* cont*/ music_chord
		  { $$=$3; lexer->pop_state(); }
	;
/*
	MUSIC
*/



transposed_music_voice:
	steno_melodic_req music_voice { 
		$$ = $2;
		$$->transpose(*$1);
		delete $1;
	}
	;

music_voice:  '{' music_voice_body '}'	{ $$ = $2; }
	| TRANSPOSE '{' transposed_music_voice '}' {
		$$ = $3;
	}
	;

music_voice_body:
	VOICE_IDENTIFIER {
		$$ = $1->mvoice(true);
	}
	| /* */ 	{
		$$ = new Music_voice;
	}
	| music_voice_body full_element {
		$$->add_elt($2);
	}
	| music_voice_body voice_command {
	}
	| music_voice_body music_chord	{
		$$->add($2);
	}
	| music_voice_body CONCAT music_voice	{
		$$->add($3);/* niet echt */
	}
	| music_voice_body error {
	}
	| music_voice_body '>' {
		fatal_error_i = 1;
		yyerror("Confused by errors: bailing out");
	};

music_chord:  '<' music_chord_body '>'	{ $$ = $2; }
	;

music_chord_body:
	CHORD_IDENTIFIER {
		$$=$1->mchord(true);
	}
	| /* */	{
		$$ = new Voice_group_chord;
	}
	| MULTIVOICE {
		$$ = new Multi_voice_chord;
	}
	| music_chord_body music_voice {
		$$->add($2);
	}
	| music_chord_body full_element {
		$$ ->add_elt($2);
	}
	| music_chord_body '}' {
		fatal_error_i = 1;
		yyerror("Confused by errors: bailing out");
	}
	| music_chord_body error {
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
 	| pre_requests lyrics_elt post_requests {
 		add_requests($2, pre_reqs);
 		add_requests($2, post_reqs);
 		$$ = $2;
        }
	| command_elt
	;

command_elt:
/* empty */ 	{
		$$ = new Voice_element;
		$$-> defined_ch_c_l_ = lexer->here_ch_c_l();
	}
/* cont: */
	command_req	{
		$2-> defined_ch_c_l_ = $$->defined_ch_c_l_;
		$$->add($2);

	}
	;

command_req:
	 '|'				{ 
		$$ = new Barcheck_req;
	}
	| BAR STRING 			{
		$$ = new Bar_req(*$2);
		delete $2;
	}
	| METER '{' int '/' int '}'	{
		Meter_change_req *m = new Meter_change_req;
		m->set($3,$5);
		// sorry hw, i need meter at output of track,
		// but don-t know where to get it... statics should go.
		// HW : default: 4/4, meterchange reqs may change it.
		
		Midi_def::num_i_s = $3;
		Midi_def::den_i_s = $5;
		$$ = m;
	}
	| SKIP '{' duration_length '}' {
		Skip_req * skip_p = new Skip_req;
		skip_p->duration_ = *$3;
		delete $3;
		$$ = skip_p;
	}
	| CADENZA '{' int '}'	{
		$$ = new Cadenza_req($3);
	}
	| PARTIAL '{' duration_length '}'	{
		$$ = new Partial_measure_req(*$3);
		delete $3;
	}
	| STEM '{' int '}'		{
		$$ = get_stemdir_req($3);
	}
	| CLEF STRING {
		$$ = new Clef_change_req(*$2);
		delete $2;
	}
	| KEY '{' pitch_list '}' 	{	
		Key_change_req *key_p= new Key_change_req;
		key_p->melodic_p_arr_ = *$3;
		$$ = key_p;
		delete $3;
	}
	| GROUPING '{' intastint_list '}' {
		$$ = get_grouping_req(*$3); delete $3;
	}
	;

post_requests:
	{
		assert(post_reqs.empty());
	}
	| post_requests post_request {
		$2->defined_ch_c_l_ = lexer->here_ch_c_l();
		post_reqs.push($2);
	}
	| post_requests close_plet_parens INT '/' INT { 
		post_reqs.push( get_request($2) ); 
		req_defined_ch_c_l = lexer->here_ch_c_l();
		post_reqs.push( get_plet_request( $2, $3, $5 ) ); 
	}
	;

post_request:
	pure_post_request
	| POST_REQUEST_IDENTIFIER	{
		$$ = $1->request(false)->clone();
	}
	;

pure_post_request:
	close_request_parens	{ 
		$$ = get_request($1); 
	}
	| script_req
	| textscript_req
	| dynamic_req
	;


octave_quote:
	'\''	 	{ $$ = 1; }
	| '`'		{ $$ = -1 ; }
	;

octave_quotes:
	/**/ { $$ = 0; }
	| octave_quotes octave_quote{ $$ += $2; }
	;

/*
	URG!!
*/
steno_melodic_req:
	MELODIC_REQUEST_IDENTIFIER	{
		$$ = $1->request(false)->clone()->melodic();
		$$->octave_i_ += lexer->prefs.default_octave_i_;
	}
	| steno_melodic_req '\'' 	{  
		$$-> octave_i_ ++;
	}
	| '`' steno_melodic_req	 {  
		$$ = $2;
		$2-> octave_i_ --;
	}
	;

steno_note_req:
	steno_melodic_req	{
		$$ = new Note_req;
		* (Melodic_req *) $$ = *$1;
		delete $1;
	}
	| steno_note_req   '!' 		{
		$$->forceacc_b_ = ! $$->forceacc_b_;
	} 
	/* have to duration here. */
	;

melodic_request:
	MELODIC '{' int int int '}'	{/* ugh */
		$$ = new Melodic_req;
		$$->octave_i_ = $3;
		$$->notename_i_ = $4;
		$$->accidental_i_ = $5;
	}
	;

dynamic_req:
	DYNAMIC '{' int '}'	{
		Absolute_dynamic_req *ad_p = new Absolute_dynamic_req;
		ad_p ->loudness_ = (Dynamic_req::Loudness)$3;
		$$ =ad_p;
	}
	;

close_plet_parens:
	']' {
		req_defined_ch_c_l = lexer->here_ch_c_l();
		$$ = ']';
	}
	;

close_request_parens:
	'('	{ 
		$$='(';
	}
	| ']'	{ 
		$$ = ']';
	}
	;
  
open_request_parens:
	')'	{ 
		$$=')';
	}
	| '['	{
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
	SCRIPT_IDENTIFIER		{ $$ = $1->script(true); }
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
		$2->defined_ch_c_l_ = lexer->here_ch_c_l();
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
		lexer->prefs.set_plet($3,$5);
	}
	| DURATIONCOMMAND '{' STRING '}'	{
		lexer->prefs.set_duration_mode(*$3);
		delete $3;
	}
	| DURATIONCOMMAND '{' notemode_duration '}'	{
		lexer->prefs.set_default_duration($3);
	}
	| OCTAVECOMMAND '{' octave_quotes '}'	{
		lexer->prefs.default_octave_i_ = $3;
	}
	| TEXTSTYLE STRING 	{
		lexer->prefs.textstyle_str_ = *$2;
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
		lexer->prefs.set_last_duration($1);
		$$[0] = $1;
		$$[1] = 0;
	}
	| INT DOTS 	{
		lexer->prefs.set_last_duration($1);
		$$[0] = $1;
		$$[1] = $2;
	}
	| DOTS  {
                lexer->prefs.get_default_duration($$);
                $$[1] = $1;
	}
	| INT '*' INT '/' INT {
		// ugh, must use Duration
		lexer->prefs.set_plet( $3, $5 );
		$$[ 0 ] = $1;
		$$[ 1 ] = 0;
		lexer->prefs.set_plet( 1, 1 );
	}
	;

default_duration:
	/* empty */	{
		lexer->prefs.get_default_duration($$);
	}
	;


voice_elt:
	steno_note_req notemode_duration 		{
		if (!lexer->note_state_b())
			yyerror("have to be in Note mode for notes");
		$$ = get_note_element($1, $2);
	}
	| RESTNAME notemode_duration		{
		$$ = get_rest_element(*$1, $2);
		delete $1;
	}
	;

lyrics_elt:
	mudela_text notemode_duration 			{
		if (!lexer->lyric_state_b())
			yyerror("Have to be in Lyric mode for lyrics");
		$$ = get_word_element($1, $2);

	};

/*
	UTILITIES
 */
pitch_list:			{
		$$ = new Array<Melodic_req*>;
	}
	| pitch_list MELODIC_REQUEST_IDENTIFIER	{
		$$->push($2->request(false)->clone()->melodic());
	}
	;

int:
	real			{
		$$ = int($1);
		if ( distance($1,Real(int($$)) ) > 1e-8)
			yyerror( "integer expected" );
	}
	;

real:
	INT			{
		$$ = Real($1);
	}
	| REAL		{
		$$ = $1;
	}
	| REAL_IDENTIFIER		{
		$$ = * $1->real(0);		
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
	lexer->LexerError(s);

	if ( fatal_error_i )
		exit( fatal_error_i );
}

void
parse_file(String init, String s)
{
   *mlog << "Parsing ... ";
   lexer = new My_lily_lexer;

#ifndef NPRINT
   yydebug = !monitor->silence("InitParser") && check_debug;
   lexer->set_debug( !monitor->silence("InitLexer") && check_debug);
#endif
	init_parse_b = true;
   lexer->new_input(init);
   yyparse();

#ifndef NPRINT
   if (!monitor->silence("InitDeclarations") && check_debug)
	lexer->print_init_declarations();

   yydebug = !monitor->silence("Parser") && check_debug;
   lexer->set_debug( !monitor->silence("Lexer") && check_debug);
#endif
	init_parse_b = false;
   lexer->new_input(s);
   yyparse();

#ifdef NPRINT
   if (!monitor->silence("Declarations") && check_debug)
	lexer->print_user_declarations();
#endif
   delete lexer;
   lexer = 0;

   if(!define_spots.empty())
	warning("Braces don't match.",0);
}

Paper_def*
default_paper()
{
    return new Paper_def(
	lexer->lookup_identifier("default_table")->lookup(true));
}


