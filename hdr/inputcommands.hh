/*
  inputcommands.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef INPUTCOMMANDS_HH
#define INPUTCOMMANDS_HH

#include "pcursor.hh"
#include "proto.hh"
#include "plist.hh"
#include "real.hh"
#include "timedescription.hh"

struct Commands_at : public IPointerList<Input_command*> {
    Time_description tdescription_;
    
    /****************/

    Moment when();
    void     parse(Staff_commands_at*);
    void print() const;
    Moment barleft();
    void add(Input_command*);
    void setpartial(Moment);
    Commands_at(const Commands_at&);
    Commands_at(Moment, Commands_at*prev);
};

struct Input_cursor : public PCursor<Commands_at*>
{
    /****************/
    Input_cursor(PCursor<Commands_at*>);
    Moment when()const;
    void find_moment(Moment w);
    void prev() { operator --(0); }
    void next() { operator ++(0); }    
};

/// the list of commands in Score
struct Input_commands : public IPointerList<Commands_at*> {    
    Input_cursor ptr;
    
    /****************/

    void find_moment(Moment);
    void add(Input_command c);
    void do_skip(int bars, Moment wholes);
        
    Input_commands();
    Input_commands(Input_commands const&);

    void reset();
    void print()const;
    Staff_commands *parse() const;
};

#endif // INPUTCOMMANDS_HH

