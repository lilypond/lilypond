/*
  midi-walker.hh -- declare Midi_walker

  (c) 1996,97 Han-Wen Nienhuys, Jan Nieuwenhuizen <jan@digicash.com>
  */

#ifndef MIDIWALKER_HH
#define MIDIWALKER_HH


#include "lily-proto.hh"

#if 0

/* 
   sketch. . .

   It would be totally cool to share code with the Engraver hierarchy,
   but this is very tough i think.
   
   */
class Performer {
    Performer_group_performer * daddy_perf_l_;
    
    virtual bool try_request(Request*r)
    {
	return daddy_perf_l_->try_request(r);
    }
    virtual void play_event( Midi_item i ) { daddy_perf_l_->play_event (i ); }
};

class Performer_group_performer : public Performer, public Translator {
    Pointer_list<Performer*> perf_p_list_;
    
    Link_array<Performer_group_performer> group_l_arr_;
    Link_array<Performer> nongroup_l_arr_;
    
    bool try_request(Request*r)
    {
	 bool hebbes_b =false;
	 for (int i =0; !hebbes_b && i < nongroup_l_arr_.size() ; i++)
	     hebbes_b =nongroup_l_arr_[i]->try_request(req_l);
	 if (!hebbes_b)
	     hebbes_b = daddy_grav_l_->try_request(req_l);
	 return hebbes_b ;
    }
    
};

class Staff_performer : public Performer_group_performer 
{
    int midi_track_i_;
    String instrument_str() { 
	return Translator::id_str_; 
    }
    virtual void play_event ( Midi_item i)
    {
	i.track_i_ = midi_track_i_;
	Performer::play_event(i);
    }
};

class Voice_performer_group_performer : public Performer_group_performer {

};

class Note_performer : public Performer {
    Melodic_req * current_l_;
    Moment switch_off_at_,switch_on_at_;


    virtual void process_request() {
	if (when() == switch_off_at_ )
 	    play_event( Note_event(current_l_->pitch()  ))
};

class Voice_performer : 
	public Performer_group_performer, public Interpreter 
{
    
};

class Score_performer: 
    public Performer_group_performer, public Global_translator 
{
    Midi_file * file_p_;
    Moment prev_;
    virtual void play_event(Midi_item i)
    {
	file_p_->output (i);
    }
    virtual void prepare(Moment m){
	file_p_->move ( m -prev_ );
    }
};

#endif

#endif // MIDIWALKER_HH


