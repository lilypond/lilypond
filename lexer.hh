#ifndef LEXER_HH
#define LEXER_HH

#include "proto.hh"

void new_input(String s);
bool close_input();
int yylex();
void yyerror(char *s);

int lookup_keyword(String s);

Identifier* lookup_identifier(String s);
void add_identifier(Identifier*i);
#endif
