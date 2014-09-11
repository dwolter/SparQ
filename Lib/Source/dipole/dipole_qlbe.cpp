/* This file is part of SparQ, a toolbox for qualitative spatial reasoning.
   Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
   More info at http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

  SparQ is free software and has been released under the terms of the GNU
  General Public License version 3 or later. You should have received a
  copy of the GNU General Public License along with this program. If not,
  see <http://www.gnu.org/licenses/>.
*/


/** 
 * Qualifier backend for dipole calculi. 
 * input format: ( (id1 sx1 sy1 ex1 ey1) (id2 sx2 sy2 ex2 ey2) 
 * (id3 sx3 sy3 ex3 ey3) ... )
 * output format: ( (id1 rel id2) (id1 rel id3) ... )
 */

#include "dipole_qlbe.h"
#include "qlbe_utils.h"
#include "qualifier.h"
#include <string>
#include <cstdlib>
	
int mode;
int type;

char cres[1024];

entity parseObject() {
  return parseDipole();
}
		
std::string getRelation(const entity& o1, const entity& o2) {

  string s = getDC_Relation_String(type, (float)o1.sx, (float)o1.sy, 
				(float)o1.ex, (float)o1.ey,
				(float)o2.sx, (float)o2.sy, 
				(float)o2.ex, (float)o2.ey);
  if (s == ":error") {
    std::cerr << "Dipole calculus not defined for the given configuration.\n";
    std::exit(1);
  }
  return s;
}
  

std::string getRelation(const entity& o1, const entity& o2, const entity& o3) {
  return "";
}

const char* dipole_qualify_24(const char*, double a, double b, double c, double d, 
	double e, double f, double g, double h)
{
	type = 1;

	entity one,two;
	vector<entity> conf;
	one.sx = a;
	one.sy = b;
	one.ex = c;
	one.ey = d;
	
	two.sx = e;
	two.sy = f;
	two.ex = g;
	two.ey = h;
	
	conf.push_back(one);
	conf.push_back(two);
	std::string desc = writeQualitativeSceneBinary(FIRST2ALL,conf);

		const char* res = (char*)desc.c_str();
    if (strlen(res)>MAX_STRBUF) {
        sprintf(cres,"OBACHT!-Stringbuffer-zu-klein!");
    } else {
        for (unsigned int i=0; i<strlen(res); i++)
            cres[i] = res[i];
    }
    //cerr << "CString: " << cres << endl;
    //cerr << "CLen: " << strlen(cres) << endl;
    return cres;
}

const char* dipole_qualify_72(const char*, double a, double b, double c, double d, 
	double e, double f, double g, double h)
{
	type = 2;

	entity one,two;
	vector<entity> conf;
	one.sx = a;
	one.sy = b;
	one.ex = c;
	one.ey = d;
	
	two.sx = e;
	two.sy = f;
	two.ex = g;
	two.ey = h;
	
	conf.push_back(one);
	conf.push_back(two);
	std::string desc = writeQualitativeSceneBinary(FIRST2ALL,conf);

		const char* res = (char*)desc.c_str();
    if (strlen(res)>MAX_STRBUF) {
        sprintf(cres,"OBACHT!-Stringbuffer-zu-klein!");
    } else {
        for (unsigned int i=0; i<strlen(res); i++)
            cres[i] = res[i];
    }
    //cerr << "CString: " << cres << endl;
    //cerr << "CLen: " << strlen(cres) << endl;
    return cres;
}

const char* dipole_qualify_80(const char*, double a, double b, double c, double d, 
	double e, double f, double g, double h)
{
	type = 3;

	entity one,two;
	vector<entity> conf;
	one.sx = a;
	one.sy = b;
	one.ex = c;
	one.ey = d;
	
	two.sx = e;
	two.sy = f;
	two.ex = g;
	two.ey = h;
	
	conf.push_back(one);
	conf.push_back(two);
	std::string desc = writeQualitativeSceneBinary(FIRST2ALL,conf);

		const char* res = (char*)desc.c_str();
    if (strlen(res)>MAX_STRBUF) {
        sprintf(cres,"OBACHT!-Stringbuffer-zu-klein!");
    } else {
        for (unsigned int i=0; i<strlen(res); i++)
            cres[i] = res[i];
    }
    //cerr << "CString: " << cres << endl;
    //cerr << "CLen: " << strlen(cres) << endl;
    return cres;
}


// main

int main(int argc, char **argv) {
  // read command line arguments to get additional parameter for the calculus
  checkArgsNum(argc, 3, 
	       "The qualifier for this calculus requires the following parameters:\n- calculus parameter string (either dipole-schlieder, dipole-coarse/dra-24, dipole-fine/dra-72/dra-69 or dipole-fine-parallelity/dra-80)\n- generation mode ('all' or 'first2all')\n"
	       );

  // determine type
  if (strcmp(argv[1],"dipole-schlieder")==0) 
    type = 0;
  else if (strcmp(argv[1],"dipole-coarse")==0 || strcmp(argv[1],"dra-24")==0)
    type = 1;
  else if (strcmp(argv[1],"dipole-fine")==0 || strcmp(argv[1],"dra-69")==0
	   || strcmp(argv[1],"dra-72")==0)
    type = 2;
  else if (strcmp(argv[1],"dipole-fine-parallelity")==0 
	   || strcmp(argv[1],"dra-80")==0)
    type = 3;
  else {
    cerr << "Wrong calculus parameter string. Should be either dipole-schlieder, dipole-coarse/dra-24, dipole-fine/dra-72/dra-69 or dipole-fine-parallelity/dra-80.\n";
    exit(1);
  }
  
  mode = readQualifierGenerationMode(argv[2]);

  // read scene description from stdin and parse it (see top of this file 
  // for input and output format)	
  std::vector<entity> confg = parseDescription();
	
  // generate qualitative description
	std::string desc = writeQualitativeSceneBinary(mode,confg);
	cout << desc << endl;
  
  return 0;	
}
