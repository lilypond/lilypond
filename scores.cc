#include "main.hh"
#include "score.hh"
#include "string.hh"

static svec<Score*> sv;

static String outfn="lelie.uit";

void
do_scores()
{
    for (int i=0; i < sv.sz(); i++) {	
	sv[i]->process();
	sv[i]->output(outfn);
	delete sv[i];
	sv[i] =0;
    }
}

void
add_score(Score * s)
{
    sv.add(s);
}


void
set_default_output(String s)
{
    outfn = s;
}

