/*
  input-file.cc -- implement Input_file

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl> Jan Nieuwenhuizen <jan@digicash.com>
*/

#include <iostream.h>
#include <strstream.h>
#include "proto.hh"
#include "plist.hh"
#include "input-file.hh"
#include "debug.hh"
#include "source-file.hh"
#include "binary-source-file.hh"
#include "source.hh"

Input_file::Input_file(String s)
{
	name = s;
	line = 1;
	String pf(s);
	if ( pf == "" ) {
		is = &cin;
		defined_ch_c_l_ = 0;
		sourcefile_l_ = 0;
	}
	else {
		Source_file* sourcefile_p = 0;
		// ugh, very dirty, need to go away
		if ( ( pf.right_str( 3 ).lower_str() == "mid" ) || ( pf.right_str( 4 ).lower_str() == "midi" ) )
		    sourcefile_p = new Binary_source_file( pf );
		else
		    sourcefile_p = new Source_file( pf );
		source_l_g->add( sourcefile_p );
		sourcefile_l_ = sourcefile_p;
		is = sourcefile_l_->istream_l();
		defined_ch_c_l_ = sourcefile_l_->ch_c_l();
	}
	cout << "[" << pf << flush;
}

Input_file::~Input_file()
{
	cout << "]" << flush;  
}
