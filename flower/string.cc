/****************************************************************************
  PROJECT: FlowerSoft C++ library
  FILE   : string.cc

  Rehacked by HWN 3/nov/95
  removed String &
  introduced Class String_handle
--*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <assert.h>
//#include "globals.hh"
#include "string.hh"

char* strlwr( char* s )
{
    char* p = s;

    while( *p )
        {
        *p = tolower( *p );    /* a macro on some compilers */
        p++;
        }
    return s;
}

char* strupr( char* s )
{
    char* p = s;

    while( *p )
        {
        *p = toupper( *p );    /* a macro on some compilers */
        p++;
        }
    return s;
}

// return array, alloced with new.
char *
String::copy_array() const
{
    const char *src = data;
    char *dest = new char[data.len() + 1];
    strcpy(dest, src);
    return dest;    
}

void
String::printOn(ostream& os) const
{
    os << (const char*) data;
}

String::String (bool b)
{
    *this = (const char *) (b ? "true" : "false");
}
String::String( const char* source )
{   
    assert(source);    
    data = source;    
}

void
String::operator +=(String s)
{
    *this += (const char *) s;
}

int
String::len() const
{
    return data.len();
}

String::String(char c,  int n)
{
    int l = n;
    assert(n >= 0 && n <= 80); // what the fuck is 80?
//min(max( n, 0 ), 80); 
    char s[81];
    memset(s, c, l);
    s[l] = 0;
    data = s;
}

String::String(int i)
{
    char digits[ 81 ];             // who the FUCK is 80???
    digits[ 0 ] = '\0';
    sprintf(digits, "%d", i );     // assume radix 10
    data = digits;
}

String::String( const int i, const int n, const char c )
{
    char fillChar = c;
    if ( fillChar)
        fillChar = '0';

    String v( i );
    
    data = String( fillChar, n - v.len() ) + String( v );
    // String convd to const char *
}

String::operator const char *() const
{
    return data;
}



#ifdef CENTRAL_OBJECT // everything derived from Sortable object
// comparisons.
int
String::operator ==( const Sortable& test ) const
{
    const String *s = (const String *) &test; 
    return *this == *s;
}

int
String::operator &&(const Object& test) const
{
    const String *s = (const String *) &test;
    
    int i = min( len(), s->len() );
    return ( i > 0 ) ?
        ( !strncmp( data, s->data, i ) ) : 0;
}

int
String::operator >( const Sortable& test ) const
{
    const String *s = (const String *) &test;
    return strcmp( data, s->data ) > 0;
}
#endif
// signed comparison,  analogous to strcmp;
int
String::compare( const char* test ) const
{
    if (test == (const char *) data)
	return 0;

    return strcmp(data, test);
}


int
String::lastPos( const char c ) const
{
    const char *me = data;
    int pos = 0;
    if ( len() )
        {
	const char* p = strrchr(me, c );
        if ( p )
            pos = p - me + 1;
        }
    return pos;
}

int
String::lastPos( const char* string ) const
{
    int pos = 0;
    int length = strlen( string );
    if ( len() && length )
        {
        int nextpos = this->pos( string );
        while( nextpos )
            {
            pos += nextpos;
            nextpos = right( len() - pos - length + 1 ).pos( string );
            }
        }
    return pos;
}

// find c
// return 0 if not found. 

// ? should return len()?, as in string.left(pos(delimiter))
int
String::pos(char c ) const
{
    const char *me = data;
    int pos = 0;
    if ( len() )
        {
	const char* p = strchr( me, c );
        if ( p )
            pos = p - me + 1;
        }
    return pos;
}

// find searchfor. (what if this == "" && searchfor == "") ???
int
String::pos( const char* searchfor ) const
{
    const char *me = data;
    int pos = 0;
    if ( len() && searchfor)
        {
	const char* p = strstr(me, searchfor);
        if ( p )
	    pos = p - me + 1;
        }
    return pos;
}

// find chars of a set.
int
String::posAny( const char* string ) const
{
    int pos = 0;
    const char *s = (const char *)data;
    if ( len() && string )
        {
	const char* p = strpbrk( s, string );
        if ( p )
	    pos = p - s + 1;
        }
    return pos;
}

String
String::left( int n ) const
{
    if (n >= len())
	return *this;

    String retval;    	
    if (n < 1)
        return retval;
    
    retval = *this;
    retval.data.trunc(n);
    return retval;
}


// n rightmst chars
String
String::right( int n ) const
{
    if (n > len())
	return *this;
    
    String retval;
    if ( n < 1)
        return retval;
    
    const char *src = (const char *)data + len() - n; 
    retval += src;

    return retval;
}


String
String::nomid( const int pos, const int n ) const
{
    String retval;
        
    if ( pos < 1 )
        return String("");
    if ( pos > len())
	return *this;
    
    return String( String( left( pos - 1 ) ) + right( len() - pos - n + 1 ));
}


String
String::mid( int pos, int n ) const
{
    String retval;

    // HWN. This SUX:
    // pos 1 == data->string[ 0 ];
    // pos 0 allowed for convenience
    if ( !len() || ( pos < 0 ) || ( pos > len() ) && ( n < 1 ) )
        return retval;

    retval = ((const char *) data) + pos -1;
    if (n > retval.len())
	n =retval.len();
    retval.data.trunc(n);
    return retval;
}


// to  uppercase
String
String::upper()
{
    char *s = data.array_for_modify();
    strupr(s );
    return *this;
}


// to lowercase
String String::lower()
{
    char  *s = data.array_for_modify();
    strlwr(s);
    return *this;
}

String::String (double f, const char *fmt)
{
    char buf[100]; // ugly
    if (!fmt)
	sprintf(buf, "%f", f);
    else
	sprintf(buf, fmt,f);
    *this = buf;
}

long
String::value() const
{
    long l =0;
    if (len()) {
	int conv = sscanf(data, "%ld", &l);
	assert(conv);
    }
    return l;
}

double
String::fvalue() const
{
    double d =0;
    if (len()) {
	int conv = sscanf(data, "%lf", &d);
	assert(conv);
    }
    return d;
}


String quoteString( String msg, String quote)
{
    return msg + " `" + quote  + "' ";
}


char *strrev(char *s)
{
  char c;
  char *p = s;
  char *q = s + strlen(s) - 1;

  while (q > p) {
    c = *p;
    *p++ = *q;
    *q-- = c;
  }
  return s;
}


String 
String::reversed() const
{
    String retval=*this;
    char  *s = retval.data.array_for_modify();
    strrev(s);
    return retval;    
}
