#include "musicalrequest.hh"
#include "paperdef.hh"
#include "lyricitem.hh"
#include "stem.hh"
#include "molecule.hh"
#include "lookup.hh"
#include "textdef.hh"
#include "sourcefile.hh"
#include "source.hh"
#include "debug.hh"
#include "main.hh"

Lyric_item::Lyric_item(Lyric_req* lreq_l, int voice_count_i)
    : Text_item(lreq_l,0)
{
    pos_i_ = voice_count_i * -4 ;	// fontsize dependant. TODO
    dir_i_ = -1;
}

void
Lyric_item::do_pre_processing()
{
    // override Text_item

    // test context-error
    if ( tdef_l_->text_str_.pos( "Gates" ) )// :-)
    	warning( "foul word", tdef_l_->defined_ch_c_l_ );
}
