/*
  lilypond, (c) 1996 Han-Wen Nienhuys
*/
#ifndef SCORECOMMANDS_HH
#define SCORECOMMANDS_HH

#include "proto.hh"
#include "command.hh"
#include "vray.hh"
#include "plist.hh"
#include "timedescription.hh"


struct Staff_commands_at : public IPointerList<Command*> {
    Time_description tdescription_;
    
    /****************/
    
    bool is_breakable();
    Moment when();
    Staff_commands_at(Time_description);
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
    Staff_commands_at*find(Moment);
    void add(Staff_commands_at*);
    void clean(Moment last);
    void OK() const;
    void print() const;
    Moment last() const;
};
/** the list of commands in Score. Put in a separate class, since it
  otherwise clutters the methods of Score.

  */

#endif

