//
// track-column.hh -- declare Track_column
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef TRACK_COLUMN_HH
#define TRACK_COLUMN_HH

/// (tcol)
class Track_column {
public:
	Track_column( Moment mom );

	void add_event( Midi_event* midi_event_p );
	Moment mom();

//private:
	IPointer_list<Midi_event*> midi_event_p_list_;
	Moment mom_;
};

#endif // TRACK_COLUMN_HH

