#include "main.hh"
#include "inputscore.hh"
#include "score.hh"
#include "string.hh"

static svec<Input_score*> sv;

static String outfn="lelie.uit";

// todo: check we don't overwrite default output.
void
do_scores()
{
    for (int i=0; i < sv.sz(); i++) {
	Score * s = sv[i]->parse();	
	delete sv[i];

	s->process();
	s->output(outfn);
	delete s;

    }
}

void
add_score(Input_score * s)
{
    sv.add(s);
}


void
set_default_output(String s)
{
    outfn = s;
}

