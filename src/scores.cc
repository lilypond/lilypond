#include "main.hh"
#include "inputscore.hh"
#include "score.hh"
#include "string.hh"
#include "paperdef.hh"
#include "debug.hh"

static Array<Input_score*> score_array_global;
String default_out_fn = "lelie";

void
do_scores()
{
    for (int i=0; i < score_array_global.size(); i++) {
	Input_score* &is_p = score_array_global[i];
	if (is_p->errorlevel_i_) {
	    warning("Score contains errors. Will not process it. ",
		    is_p->defined_ch_c_l_);
	    delete is_p;
	    continue;
	} 
	
	if (only_midi) {
	    delete is_p->paper_p_;
	    is_p->paper_p_ = 0;
	}

	Score * s_p = is_p->parse();	
	delete is_p;
	s_p->print ();
	s_p->process();
	delete s_p;
    }
    score_array_global.set_size(0);
}

void
add_score(Input_score * s)
{
    score_array_global.push(s);
}

void
set_default_output(String s)
{
    default_out_fn = s;
}

