// stringutil.cc
#ifndef __STRING_UTIL_CC
#define __STRING_UTIL_CC

// include only when 
// *  reading stringutil.hh, or 
// *  string util stuff not inlined
#if !defined STRING_UTILS_INLINED

#include <assert.h>
#include <memory.h>
#include "stringutil.hh"


// namespace StringData { namespaces are broken in this version of g++ 
// gcc version 2.7.2.1


StringData::StringData() 
{
    references=0;
    maxlen = INITIALMAX;
    data_by_p_ = new Byte[maxlen + 1];
    data_by_p_[0] = 0;
    length_i_ = 0;
}

StringData::StringData(StringData const &src) 
{
    references=0;	
    maxlen = length_i_ = src.length_i_;		
    data_by_p_ = new Byte[maxlen+1]; // should calc GNU 8byte overhead. 	
    memmove( data_by_p_, src.data_by_p_, length_i_ + 1 );	
}


StringData::~StringData() 
{
    assert(references == 0);
    delete[] data_by_p_;
}

void 
StringData::setmax(int j) 
{	
    OKW();
    if (j > maxlen) {
	delete data_by_p_;
	maxlen = j;
	data_by_p_ = new Byte[maxlen + 1];
    
	data_by_p_[0] = 0;
	length_i_ = 0;
    }
}


void 
StringData::remax(int j) 
{
    OKW();
    if (j > maxlen) {
	maxlen = j;
	Byte *p = new Byte[maxlen + 1];	    
	memmove( p, data_by_p_, length_i_ + 1 );	    
	delete[] data_by_p_;
	data_by_p_ = p;
    //    length_i_ = strlength_i(data_by_p_);
    }
}


void 
StringData::OKW() 
{
    assert (references == 1);
}

void 
StringData::OK() 
{
// can-t do this with binary data
//	assert(strlen(data_by_p_) == size_t(length_i_));
    assert(maxlen >= length_i_);
    assert(bool(data_by_p_));
    assert(references >= 1);
}

//    // needed?
// well, we can't -> depreciated
//    void update() {
//	length_i_  = strlen (data_by_p_);
//    }


void 
StringData::tighten() 
{ // should be dec'd const
    maxlen = length_i_;
    Byte *p = new Byte[maxlen + 1];	    
    memmove( p, data_by_p_, length_i_ + 1 );	    
    delete[] data_by_p_;
    data_by_p_ = p;		
}

// assignment.
void 
StringData::set( Byte const* by_c_l, int length_i ) 
{
    OKW();

    assert( by_c_l && by_c_l != data_by_p_);

    length_i_ = length_i;
    remax( length_i_ );
    memmove( data_by_p_, by_c_l, length_i_ );
    data_by_p_[ length_i_ ] = 0;
}


void 
StringData::set( char const* ch_c_l ) 
{
    set( (Byte const*)ch_c_l, strlen( ch_c_l ) );
}


/// concatenation.
void 
StringData::append( Byte const* by_c_l, int length_i ) 
{
    OK();
    OKW();
    int old_i = length_i_;
    
    length_i_ += length_i;
    remax( length_i_ );
    memmove( data_by_p_ + old_i, by_c_l, length_i );	
    data_by_p_[ length_i_ ] = 0;
}


void 
StringData::operator += ( char const* ch_c_l ) 
{
    append( (Byte const*)ch_c_l, strlen( ch_c_l ) );
}

char const*
StringData::ch_c_l() const
{
    return (char const*)data_by_p_; 
}

char* 
StringData::ch_l() 
{ 
    return (char*)data_by_p_; 
}

Byte const*
StringData::by_c_l() const 
{ 
    return data_by_p_; 
}

// idem, non const
Byte* 
StringData::by_l() 
{
    OKW();
    return data_by_p_;
}

void 
StringData::trunc(int j) 
{
    OKW(); 
    assert(j >= 0 && j <= length_i_);
    data_by_p_[j] = 0;
    length_i_ = j;
}

Byte&
StringData::operator [](int j) 
{
    assert(j >= 0 && j <= length_i_);
    return data_by_p_[j] ; 
}

Byte 
StringData::operator [](int j) const 
{
    assert(j >= 0 && j <= length_i_);
    return data_by_p_[j]; 
}


// } namespace broken




// namespace String_handle {


void 
String_handle::down() 
{ 
    if (!(--data->references)) delete data; data = 0; 
}

/// increase ref count
void 
String_handle::up(StringData *d) 
{ 
    data=d; data->references ++; 
}

void 
String_handle::copy() 
{
    if (data->references !=1){
	StringData *newdata = new StringData(*data);
	down();
	up(newdata);
    }
}

String_handle::String_handle() 
{
    up(new StringData);
}
String_handle::~String_handle() 
{	
    down();
}    
String_handle::String_handle(String_handle const & src) 
{	
    up(src.data);
}

Byte* 
String_handle::by_l() 
{
    copy();
    return data->by_l();
}

char* 
String_handle::ch_l() 
{
    copy();
    return (char*)data->by_l();
}

Byte 
const* String_handle::by_c_l() const 
{
    return data->by_c_l();
}

char const* 
String_handle::ch_c_l() const 
{
    return (char const*)data->by_c_l();
}

void 
String_handle::operator =(String_handle const &src) 
{
    if (this == &src)
	return;
    down();
    up(src.data);
}

void 
String_handle::operator += (char const *s) 
{	
    copy();
    *data += s;
}    


Byte 
String_handle::operator[](int j) const 
{ 
    return (*data)[j]; 
}

// !NOT SAFE!
// don't use this for loops. Use by_c_l()
Byte &
String_handle::operator[](int j) 
{
    copy(); 	// hmm. Not efficient
    return data->by_l()[j];
}

void 
String_handle::append( Byte const* by_c_l, int length_i ) 
{
    copy();
    data->append( by_c_l, length_i );
}
			   
void 
String_handle::set( Byte const* by_c_l, int length_i ) 
{
    copy();
    data->set( by_c_l, length_i );
}
			   
void 
String_handle::operator = (char const *p) 
{
    copy();
    data->set( p );
}
			   
void 
String_handle::trunc(int j) 
{
    copy(); data->trunc(j); 
}

int 
String_handle::length_i() const 
{ 
    return data->length_i_; 
}


// } namespaces broken


#endif // not STRING_UTILS_INLINED //

#endif // __STRING_UTIL_CC //
