/*
  lilypond, (c) 1996 Han-Wen Nienhuys
*/
#ifndef SCOMMANDS_HH
#define SCOMMANDS_HH

#include "proto.hh"
#include "command.hh"
#include "vray.hh"
#include "list.hh"

struct Score_commands : public      PointerList<Command*> {
    void add(Command);
    void add_seq(svec<Command>);
    void clean(Real last);
    void set_breakable(Real when);
    bool is_breakable(Real w);
    PCursor<Command*> last_insertion(Real w);
    PCursor<Command*> first(Real w);
    void add_command_to_break(Command pre, Command mid,Command post);
    void OK() const;
    void print() const;
};
/** the list of commands in Score. Put in a separate class, since it
  otherwise clutters the methods of Score.  */

#endif

