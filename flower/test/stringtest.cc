/*
  stupid test program to verify stringlib
  stringtest.cc
  */
#include <iostream.h>
#include "string.hh"
#include "varray.hh"
#include "string-convert.hh"

void
ctors()
{
    cout << "constructors"<<endl;

    String str( "hai" );
    String def;
    String fromi(10);
    String fromc('c');
    String fromf(1.32e-2, "%g");

    cout << str << endl;
    cout << def << endl;
    cout << fromi<< endl;
    cout << fromc<< endl;       
    cout << fromf<< endl;
}

void
cmp()
{
    Array<String> a;
    a.push("abcd");
    a.push("zxy");
    a.push("abc");
    a.push("");
    a.sort(String::compare_i);
    cout << "compares: "<<endl;
    for (int i=0; i < a.size(); i++)
	cout << a[i] << endl;
}


void
searching()
{
    String hay = "foobarbazblub";

    char c =   'b';
    String cstr =c;
    String set = "bar";
    cout << "hay = \"" << hay << "\" len="<< hay.length_i()<<endl;
    cout << "index_i('"<< c<<"') " << c << "= " << hay.index_i(c) <<endl;
    cout << "last_index_i('"<< c<<"') " << c << "= " << hay.index_last_i(c) <<endl;    
//    cout << "last index of cstr " << c << ": " << hay.index_last_i(cstr) <<endl;    
//    cout << "index_last_i(\""<<set<<"\"): " << hay.index_last_i(set) <<endl;
    cout << "index_i(\""<<set<<"\"): " << hay.index_i(set) <<endl;    
    cout << "index_any(\"" << set << "\"): " << cstr << ": " << hay.index_any_i(cstr) <<endl;

    
    
}


void
kutenpeer()
{
    String str( "hai" );
    for (int i=-1; i < str.length_i()+2; i++) {
	cout<<" left_str(" << i<<"): " << str.left_str( i ) << endl;
	cout<<" right_str( "<<i<<"): " << str.right_str( i ) << endl;
    }
    str = "blonde haren";
    cout << str<<endl;
    cout << "mid(2,6)="<<str.mid_str(2,6)<<endl;
    cout << "nomid(2,6)="<<str.nomid_str(2,6)<<endl;
}

bool
test_empty_b( String str )
{
    cout << "`" << str << "' is ";

    if ( str == String( "" ) ) {
        cout << "empty" << endl;
	return true;
    }

    cout << "not empty" << endl;
    return false;
}

int 
main()
{
    ctors();
    cmp();
    searching();
    kutenpeer();
    String str( "hai" );
    cout <<  str << endl;
    cout << "left" << endl;
    str += " daar";
    cout << str << endl;

//    str = String( "Hallo" ) + " daaR" + '!'; // no go on doze-s gcc2.7.2?
    str = String( "Hallo" ) + " daaR" + "!";
    cout << str << endl;

    cout << "up: " << str.upper_str() << " down: " << str.lower_str()<<endl;
    
    if ( test_empty_b( str ) )
    	return 1;
    
    String fn = "";
    if ( !test_empty_b( fn ) )
    	return 1;
    
    fn = "";
    fn += "";
    delete fn.copy_byte_p();
    delete str.copy_byte_p();

    cout << String_convert::bin2hex_str( String( (char)0xff ) ) << endl;
    cout << "-1:" << String_convert::i2hex_str( -1, 2, '0' );
    cout << endl;
    return 0;
}


