#include "musical-request.hh"
#include "paper-def.hh"
#include "lyric-item.hh"
#include "stem.hh"
#include "molecule.hh"
#include "lookup.hh"
#include "text-def.hh"
#include "source-file.hh"
#include "source.hh"
#include "debug.hh"

Lyric_item::Lyric_item(Lyric_req* lreq_l, int voice_count_i)
    : Text_item(lreq_l,0)
{
    pos_i_ = -voice_count_i * 4 ;	// 4 fontsize dependant. TODO
    dir_i_ = -1;
}

void
Lyric_item::do_pre_processing()
{

    // test context-error
    if ( tdef_l()->text_str_.index_i( "Gates" ) >=0)// :-)
    	warning( "foul word", tdef_l()->defined_ch_C_ );
}
