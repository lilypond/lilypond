/*
  lilypond, (c) 1996 Han-Wen Nienhuys
*/
#ifndef SCOMMANDS_HH
#define SCOMMANDS_HH

#include "proto.hh"
#include "command.hh"
#include "vray.hh"
#include "plist.hh"

/// the list of commands in Score
struct Score_commands : public PointerList<Command*> {
    void process_add(Command);
    Score_commands*parse(Real last)const;
    void parser_add(Command*);
    void add_seq(svec<Command>);
    void clean(Real last);
    void set_breakable(Real when);
    bool is_breakable(Real w);
    PCursor<Command*> last_insertion(Real w);
    PCursor<Command*> first(Real w);
    void add_command_to_break(Command pre, Command mid,Command post);
    void OK() const;
    void print() const;
    Real last() const;
    void insert_between(Command victim, PCursor<Command*> firstc,
			PCursor<Command*> last);
};
/** the list of commands in Score. Put in a separate class, since it
  otherwise clutters the methods of Score.

  This class serves two purposes: it stores the commands (via
  parser_add()) in the yacc parser. at a later stage, some 'high
  level' commands are converted (method: parse())  
  */

#endif

