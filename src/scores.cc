#include "main.hh"
#include "inputscore.hh"
#include "score.hh"
#include "string.hh"

static Array<Input_score*> score_array_global;

static String outfn="lelie.out";

// todo: check we don't overwrite default output.
void
do_scores()
{
    for (int i=0; i < score_array_global.size(); i++) {
	Score * s_p = score_array_global[i]->parse();	
	delete score_array_global[i];
	s_p->print ();
	s_p->process();
	s_p->output(outfn);
	delete s_p;
    }
    score_array_global.set_size(0);
}

void
add_score(Input_score * s)
{
    score_array_global.push(s);
}

#if 0
Input_score*
current_iscore_l()
{
    if ( score_array_global.size() )
	return score_array_global.last(); // UGH
    else
    	return 0;
}
#endif

void
set_default_output(String s)
{
    outfn = s;
}

