/*
  lilypond, (c) 1996 Han-Wen Nienhuys
*/
#ifndef SCORECOMMANDS_HH
#define SCORECOMMANDS_HH

#include "proto.hh"
#include "command.hh"
#include "vray.hh"
#include "plist.hh"


struct Staff_commands_at : public IPointerList<Command*> {
    Real when;

    /****************/
    
    bool is_breakable();

    Staff_commands_at(Real);
    void set_breakable();
    void add_command_to_break(Command pre, Command mid,Command post);
    void print() const;
    void OK() const;
    void insert_between(Command victim, PCursor<Command*> firstc,
			PCursor<Command*> last);
    void add(Command c);

};

/// the list of commands in Score
struct Staff_commands : public IPointerList<Staff_commands_at*>
{
    Staff_commands_at*find(Real);
    void add(Command,Real);
    void clean(Real last);
    void OK() const;
    void print() const;
    Real last() const;
};
/** the list of commands in Score. Put in a separate class, since it
  otherwise clutters the methods of Score.

  */

#endif

