/*
  input.cc -- implement Input

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "proto.hh"
#include "input.hh"
#include "string.hh"
#include "source.hh"
#include "source-file.hh"

Input::Input(Sources*s, char const *cl)
{
    sources_l_=s;
    defined_ch_C_=cl;
}

Input::Input()
{
    sources_l_ = 0;
    defined_ch_C_ = 0;
}

Input::Input(Input const &s)
{
    sources_l_ = s.sources_l_;
    defined_ch_C_ = s.defined_ch_C_;
}

void
Input::set_spot(Input const &i)
{
    *this  = i;
}

void
Input::message(String message_str)const
{
    String str = "";
    Source_file* sourcefile_l=0;
    
    if (sources_l_) 
	sourcefile_l = sources_l_->sourcefile_l( defined_ch_C_ );
    
    if ( sourcefile_l ) {
	str += sourcefile_l->file_line_no_str(defined_ch_C_) + String(": ");
    }
    
    str += message_str;
    if ( sourcefile_l ) {
	str += ":\n";
	str += sourcefile_l->error_str( defined_ch_C_);
    }
    /*
    if ( busy_parsing() )
    	cerr << endl;
	*/
    cerr << str << endl;
}

void
Input::warning( String message_str)const
{
    message( "warning: " + message_str);
}
void
Input::error(String s)const
{
    message("error: "+ s);
    exit (1);
}

String
Input::location_str()const
{
    Source_file * sourcefile_l=0;
    if (sources_l_)
	sourcefile_l = sources_l_->sourcefile_l (defined_ch_C_);
    if (sourcefile_l) 
	return sourcefile_l->file_line_no_str(defined_ch_C_);
    else
	return "(location unknown)";
}
