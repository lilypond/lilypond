/*
  input-file.hh -- declare Input_file

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef INPUT_FILE_HH
#define INPUT_FILE_HH
#include <FlexLexer.h>
#include "proto.hh"
#include "fproto.hh"
#include "varray.hh"
#include "string.hh"

struct Input_file {
	istream* is;
	char const* defined_ch_c_l_;
	Source_file* sourcefile_l_;
	int line;
	String name;

	Input_file(String);
	~Input_file();
};

#endif // INPUT_FILE_HH
