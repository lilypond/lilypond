%{ // -*-Fundamental-*-
#include <iostream.h>

#include "lexer.hh"
#include "paper.hh"
#include "staff.hh"
#include "score.hh"
#include "main.hh"
#include "keyword.hh"
#include "scommands.hh"
#include "debug.hh"
#include "parseconstruct.hh"
#include "dimen.hh"
#include "identifier.hh"

#ifndef NDEBUG
#define YYDEBUG 1
#endif

%}


%union {    
    Real real;
    Command *command;
    Identifier *id;    
    Score_commands *scommands;
    Voice *voice;    
    Voice_element *el;	
    Staff *staff;    
    String *string;
    Score *score;
    const char *consstr;
    Paperdef *paper;
    int i;
}

%token VOICE STAFF SCORE TITLE RHYTHMSTAFF BAR NOTENAME OUTPUT
%token CM IN PT MM PAPER WIDTH METER UNITSPACE SKIP COMMANDS
%token MELODICSTAFF

%type <consstr> unit
%token <id>  IDENTIFIER
%token <string> NEWIDENTIFIER 
%token <string> PITCH DURATION RESTNAME
%token <real> REAL
%token <string> STRING
%type <id> declaration 
%type <paper> paper_block paper_body
%type <real> dim
%type <voice> voice_block voice_body voice_elts voice_elts_dollar
%type <el> voice_elt
%type <command> score_command
%type <score> score_block score_body
%type <staff> staff_block  rhythmstaff_block rhythmstaff_body
%type <staff> melodicstaff_block melodicstaff_body staffdecl
%type <i> int
%type <scommands> score_commands_block score_commands_body

%%

mudela:	/* empty */
	| mudela score_block { 
		add_score($2);
	}
	| mudela add_declaration {	}
	;

add_declaration: declaration	{
		add_identifier($1);
	}
	;

declaration:
	NEWIDENTIFIER '=' staff_block  {
		$$ = new Staff_id(*$1, $3);
		delete $1; // this sux
	}
	;


score_block: SCORE '{' score_body '}' 	{ $$ = $3; }
	;

score_body:		{ $$ = new Score; } 
	| score_body staff_block	{ $$->add($2); }
	| score_body score_commands_block 	{ $$->set($2); }
	| score_body paper_block		{ $$->set($2);	}
	;
score_commands_block:
	COMMANDS '{' score_commands_body '}' { $$ =$3;}
	;

score_commands_body:			{ $$ = new Score_commands; }
	| score_commands_body score_command		{
		$$->parser_add($2);
	}
	;

paper_block:
	PAPER '{' paper_body '}' 	{ $$ = $3; }
	;

paper_body:
	/* empty */		 	{ $$ = new Paperdef; }
	| paper_body WIDTH dim		{ $$->linewidth = $3;}
	| paper_body OUTPUT STRING	{ $$->outfile = *$3;
		delete $3;
	}
	| paper_body UNITSPACE dim	{ $$->whole_width = $3; }
	;

dim:
	REAL unit	{ $$ = convert_dimen($1,$2); }
	;


unit:	CM		{ $$ = "cm"; }
	|IN		{ $$ = "in"; }
	|MM		{ $$ = "mm"; }
	|PT		{ $$ = "pt"; }
	;
	
/*
	staff
*/
staff_block:
	staffdecl
	| rhythmstaff_block
	| melodicstaff_block
	;

staffdecl: STAFF '{' IDENTIFIER '}' { $$ = $3->staff(); }
	;

rhythmstaff_block:
	RHYTHMSTAFF '{' rhythmstaff_body '}'	{ $$ = $3; }
	;

rhythmstaff_body:
	/* empty */			{ $$ = get_new_rhythmstaff(); }
	| rhythmstaff_body voice_block 	{ $$->add_voice($2); } 	
	;

melodicstaff_block:
	MELODICSTAFF '{' melodicstaff_body '}'	{ $$ = $3; }
	;

melodicstaff_body:
	/* empty */			{ $$ = get_new_melodicstaff(); }
	| melodicstaff_body voice_block 	{ $$->add_voice($2); } 	
	;

/*
	voice
*/
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
	SKIP int ':' REAL		{
		$$ = get_skip_command($2, $4);
	}
	| METER  int int		{
		$$ = get_meterchange_command($2, $3);
	}
/*	| PARTIALMEASURE REAL		{
		$$ = get_partial_command($2);
	}*/
	;
	

int:
	REAL			{
		$$ = int($1);
		if (ABS($1-Real(int($$))) > 1e-8)
			yyerror("expecting integer number");
		
	}
	;

%%

void
parse_file(String s)
{
   *mlog << "Parsing ... ";
#ifdef YYDEBUG
   yydebug = !monitor.silence("Parser");
#endif
   new_input(s);
   yyparse();
   *mlog << "\n";
}
