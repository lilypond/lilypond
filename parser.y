%{ // -*-Fundamental-*-
#include <iostream.h>
//#include "mudobs.hh"
#include "lexer.hh"
#include "staff.hh"
#include "score.hh"
#include "keyword.hh"
#include "globvars.hh"
#include "debug.hh"
#include "parseconstruct.hh"
#define YYDEBUG 1


%}


%union {
     int i;    
    Real real;
    Command *command;
    Identifier *id;    

    Voice *voice;    
    Voice_element *el;	
    Staff *staff;    
    String *string;
    Score *score;    
}

%token VOICE STAFF SCORE TITLE RHYTHMSTAFF BAR NOTENAME

%token <id> IDENTIFIER
%token <string> PITCH DURATION RESTNAME
%token <real> REAL


%type <voice> voice_block voice_body voice_elts voice_elts_dollar
%type <el> voice_elt
%type <command> score_command
%type <score> score_block score_body
%type <staff> staff_block  rhythmstaff_block rhythmstaff_body

%%

mudela:
	score_block { 
		delete the_score; 
		the_score = $1;
	}
	;


score_block: SCORE '{' score_body '}' 	{ $$ = $3; }
	;

score_body:		{ $$ = new Score; } 
	| score_body staff_block	{ $$->add($2); }
	| score_body score_command	{ $$->add($2); }
	;

staff_block:
	rhythmstaff_block
	;

rhythmstaff_block:
	RHYTHMSTAFF '{' rhythmstaff_body '}'	{ $$ = $3; }
	;

rhythmstaff_body:
	/* empty */			{ $$ = get_new_rhythmstaff(); }
	| rhythmstaff_body voice_block 	{ $$->add_voice($2); } 	
	;

voice_block:
	VOICE '{' voice_body '}'	{ $$ = $3; }
	;


voice_body:
	REAL voice_elts_dollar { $$ = $2; $$->start = $1; }
	| voice_elts_dollar	{ $$ = $1; }
	;

voice_elts_dollar:
	'$' voice_elts '$'  { $$ = $2; }
 	;

voice_elts:
	/* empty */		{
            $$ = new Voice;
        }
        | voice_elts voice_elt {
            $$->add($2);
        }
	;

voice_elt:
	PITCH DURATION 			{ $$ = get_note_element(*$1, *$2);

	}
	|  RESTNAME DURATION		{ $$ = get_rest_element(*$1, *$2);

	}
	;

score_command:
	BAR REAL			{
		$$ = get_bar_command($2);
	}
	;

%%

void
parse_file(String s)
{
   *mlog << "Parsing ... ";
   yydebug = debug_flags & DEBUGPARSER;
   new_input(s);
   yyparse();
}
