//
//  binary-source-file.hh -- declare Binary_source_file
//
//  copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef BINARY_SOURCE_FILE_HH
#define BINARY_SOURCE_FILE_HH

#include "source-file.hh"

class Binary_source_file : public Source_file {
public:
	Binary_source_file (String& filename_str );
	virtual ~Binary_source_file ();

	virtual String error_str (char const* pos_ch_c_l ) const;
	virtual int line_i (char const* pos_ch_c_l ) const;
};

#endif // BINARY_SOURCE_FILE_HH //
