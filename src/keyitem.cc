#include "keyitem.hh"
#include "key.hh"
#include "debug.hh"
#include "molecule.hh"
#include "paper.hh"
#include "lookup.hh"
#include "clef.hh"

const int FLAT_TOP_PITCH=2; /* fes,ges,as and bes typeset in lower octave */
const int SHARP_TOP_PITCH=4; /*  ais and bis typeset in lower octave */
NAME_METHOD(Keyitem);
Keyitem::Keyitem(int c)
{
    c_position = c;
}

void
Keyitem::read(Array<int> s)
{
    for (int i = 0 ; i< s.size(); ) {
	int note = s[i++];
	int acc = s[i++];
	    
	add(note, acc);
    }
}

void 
Keyitem::read(const Clef& c)
{
    c_position=(c.c0_pos+70)%7;
}


void
Keyitem::add(int p, int a)
{
    if ((a<0 && p>FLAT_TOP_PITCH) ||
        (a>0 && p>SHARP_TOP_PITCH)) {
      p=p-7; /* Typeset below c_position */
    }
    pitch.push(p);
    acc.push(a);
}


Molecule*
Keyitem::brew_molecule_p()const
{
    Molecule*output = new Molecule;
    Real inter = paper()->interline()/2;
    
    for (int i =0; i < pitch.size(); i++) {
	Symbol s= paper()->lookup_p_->accidental(acc[i]);
	Atom a(s);
	a.translate(Offset(0,(c_position + pitch[i]) * inter));
	Molecule m(a);
	output->add_right(m);	
    }
    Molecule m(paper()->lookup_p_->fill(Box(
	Interval(0, paper()->note_width()),
	Interval(0,0))));
    output->add_right(m);
    return output;
}
