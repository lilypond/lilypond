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
#include "moment.hh"

struct Commands_at : public IPointerList<Input_command*> {
    Moment moment_;
    
    /****************/

    Real when();
    void     parse(Staff_commands_at*);
    void print() const;
    Real barleft();
    void add(Input_command*);
    void setpartial(Real);
    Commands_at(const Commands_at&);
    Commands_at(Real, Commands_at*prev);
};

struct Input_cursor : public PCursor<Commands_at*>
{
    /****************/
    Input_cursor(PCursor<Commands_at*>);
    Real when()const;
    void find_moment(Real w);
    void prev() { operator --(0); }
    void next() { operator ++(0); }    
};

/// the list of commands in Score
struct Input_commands : public IPointerList<Commands_at*> {    
    Input_cursor ptr;
    
    /****************/

    void find_moment(Real);
    void add(Input_command c);
    void do_skip(int bars, Real wholes);
        
    Input_commands();
    Input_commands(Input_commands const&);

    void reset();
    void print()const;
    Staff_commands *parse() const;
};



void
interpret_meter(Input_command *c, int &beats_per_meas, int& one_beat,
		Real& whole_per_measure);
#endif // INPUTCOMMANDS_HH

