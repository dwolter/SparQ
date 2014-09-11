#ifndef _QTC_INTERFACE_H_
#define _QTC_INTERFACE_H_

#include "QTC.h"
#include <stdlib.h>
#include <string>
#include <iostream>

struct relation
{
  char c[6];
  int  length;
};

const char* return_result(std::string);
void parseRelation(const char*, struct relation*);

// *** Velocity ***
string qtc_general_velocity_qualify( float, float);
string qtc_general_velocity_composition( string, string);
char qtc_general_velocity_converse( char vel);

// *** Angle ***
// Currently no composition of angle labels known (compare PhD of N.v.d.Weghe)
// probably not possible without "DISTANCE"
// => every time "0"
string qtc_general_angle_qualify( float, float);
string qtc_general_angle_composition( string, string);
string qtc_general_angle_converse( string);

#define MAX_STRBUF 1024
#define N_C22 6
//#define C22 1
#define N_C21 4
//#define C21 2
#define N_B22 3
//#define B22 3
#define N_B21 2
//#define B21 4



// QTC-C22
extern "C" const char* qtc_c22_qualify(const char*, const char*, const char*);
extern "C" const char* qtc_c22_composition( const char*, const char*, const char*);
extern "C" const char* qtc_c22_converse(const char*, const char*);

// QTC-C21
extern "C" const char* qtc_c21_qualify(const char*, const char*, const char*);
extern "C" const char* qtc_c21_composition(const char*, const char*, const char*);
extern "C" const char* qtc_c21_converse(const char*, const char*);

// QTC-B22
extern "C" const char* qtc_b22_qualify(const char*, const char*, const char*);
extern "C" const char* qtc_b22_composition(const char*, const char*, const char*);
extern "C" const char* qtc_b22_converse(const char*, const char*);

// QTC-B21
extern "C" const char* qtc_b21_qualify(const char*, const char*, const char*);
extern "C" const char* qtc_b21_composition(const char*, const char*, const char*);
extern "C" const char* qtc_b21_converse(const char*, const char*);

extern char cres[];

#endif
