//
// duration.hh -- declare Duration, Plet, Duration_convert Duration_iterator
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

// split into 4?

#ifndef DURATION_HH
#define DURATION_HH

// ugh, to get me in lily lib
extern bool no_triplets_bo_g;

/// (dur)
struct Duration {
	// actually i hate it when other people use default arguments,
	// because it makes you easily loose track of what-s really
	// happening; in the routine-s implementation you-re not aware
	// of this defaultness (who sets this stupid value?).
	Duration( int type_i = 1, int dots_i = 0, Plet* plet_p = 0 );
	Duration( Duration const& dur_c_r );
	~Duration();

	Duration const& operator =( Duration const& dur_c_r );

	void set_plet( Plet* plet_l ); // handiger: newt zelf

//	    int i_;	// balltype -> type!
	int type_i_;
	int dots_i_;
	Plet* plet_p_;
};

/// (plet)
struct Plet {
	Plet( int replace_i, int type_i );
	Plet( Plet const& plet_c_r );

//	    int i_;
	int iso_i_;  // 2/3; 2 is not duration, maar of count!
	int type_i_;
};

/**
	Duration_convert handles all conversions to -n fro Duration (dur).
	That is including (integer + division) representation for MIDI,
	and conversion from unexact time representation (best guess :-).

	A Moment (mom) is a Rational that holds the time fraction 
	compared to a whole note (before also called wholes).

	SUGGESTION: currently a moment in time is called moment too;
	let-s typedef Rational When too, so that we get 
	When Staff_column::when(), Moment Voice_element::mom().
*/
struct Duration_convert {
	/// Most used division in MIDI, all fine with me.
	static int const division_1_c_i = 384;

	/// Return (integer, division) representation.
	static int dur2_i( Duration dur, int division_1_i = division_1_c_i );
	
	/// Return Moment representation (fraction of whole note).
	static Moment dur2_mom( Duration dur );

	/// Return Mudela string representation.
	static String dur2_str( Duration dur );

	/// Return Moment from (integer, division) representation.
	static Moment i2_mom( int i, int division_1_i = division_1_c_i );

	/// Return Moment (fraction of whole) representation, best guess.
	static Duration mom2_dur( Moment mom );

	/// Return plet factor (not a Moment: should use Rational?).
	static Moment plet_factor_mom( Duration dur );

	/** Return synchronisation factor for mom, so that
	    mom2_dur( mom / sync_f ) will return the duration dur.		
	*/ 
	static Real sync_f( Duration dur, Moment mom );
};

/// (iter_dur)
struct Duration_iterator {
	/// start at shortest: 128:2/3
	Duration_iterator();

	// **** what about these three here ?
	/// return forward_dur();
 	Duration operator ++(int); 

	/// return ok()
 	operator bool(); 

	/// return dur()
 	Duration operator ()(); 
	// ****

	/// return current dur
	Duration dur();

	/// return dur(), step to next
	Duration forward_dur();

	/// durations left?
	bool ok();

private:
	Duration cursor_dur_;
};

#endif // DURATION_HH

