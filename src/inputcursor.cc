/*
  it still sucks.
  */

#include "inputcommands.hh"
#include "inputcommand.hh"
#include "debug.hh"
#include "staffcommands.hh"
#include "getcommand.hh"
#include "command.hh"

void
interpret_meter(Input_command *c, int &beats_per_meas, int& one_beat,
		Real& whole_per_measure)
{
    beats_per_meas = c->args[1].value();
    one_beat = c->args[2].value();
    whole_per_measure = beats_per_meas/Real(one_beat);
}

Real
Input_cursor::when()const
{
    return (*this)->when; 
}

void
Input_cursor::print() const
{
#ifndef  NPRINT
    mtor << "meter " << whole_per_measure
	 << " pos "<< bars << ":" << whole_in_measure <<'\n';
#endif
}
	
void
Input_cursor::reset()
{
    whole_per_measure = 1.0;	// ?
    whole_in_measure =0.0;
    bars = 0;
    last=0;    
}

Input_cursor :: Input_cursor(PCursor<Input_command*>c)
    :PCursor<Input_command*>(c)
{
    reset();    
}

void
Input_cursor::sync()
{
    assert(ok());
    
    whole_in_measure += when() - last;
    while (whole_per_measure > 0 && whole_in_measure >= whole_per_measure) {
	bars ++;
	whole_in_measure -= whole_per_measure;	
    }
    if (whole_in_measure < 1e-5) // ugr
	whole_in_measure = 0.0;
}

void
Input_cursor::operator++(int)
{    
    last = when();
    (*(PCursor<Input_command*> *) this) ++;

    if (ok()) {	
	sync();
	if (ptr()->args[0] == "METER") {
	    int i,j;	    
	    interpret_meter(ptr(), i, j, whole_per_measure);
	}
    }
}

void
Input_cursor::addbot(Input_command*c)
{
    assert(!ok());    
    add(c);
}


void
Input_cursor::add(Input_command*c)
{
    PCursor<Input_command*> ::add(c);
    (*this)++;
}

void
Input_cursor::last_command_here()
{
    assert(ok());
    PCursor<Input_command*> next = (*this)+1;
    while (next.ok() && next->when == when()){
	*this = next;
	next = *this +1;
	
    }
}

void
Input_cursor::setpartial(Real p)
{
    if (when())
	error_t ("Partial measure only allowed at beginning.", when() );
    if (p<0||p > whole_per_measure)
	error_t ("Partial measure has incorrect size", when());

    whole_in_measure = whole_per_measure - p;	
}
