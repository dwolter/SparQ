/* This file is part of SparQ, a toolbox for qualitative spatial reasoning.
   Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
   More info at http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

  SparQ is free software and has been released under the terms of the GNU
  General Public License version 3 or later. You should have received a
  copy of the GNU General Public License along with this program. If not,
  see <http://www.gnu.org/licenses/>.
*/


/** 
 * Qualifier backend for double cross calculus with single numbering (dcc2). 
 * input format: ( (id1 x1 y1) (id2 x2 y2) (id3 x3 y3) ... )
 * output format: ( (id1 id2 rel id3) (id2 id3 rel id4) ... )
 */

#include "qlbe_utils.h"
#include "qualifier.h"
#include "dcc2_qlbe.h"

#include <string>
#include <cstdlib>
	
int mode;

char cres[1024];


entity parseObject() {
  return parsePoint();
}
		
std::string getRelation(const entity& o1, const entity& o2) {
  return "";
}

std::string getRelation(const entity& o1, const entity& o2, const entity& o3) {
  string s = getDCC2_Relation_String(o1.x, o1.y,
				     o2.x, o2.y,
				     o3.x, o3.y);

  if (s == ":error") {
    std::cerr << "DCC 2 calculus not defined for the given configuration.\n";
    std::exit(1);
  }

  return s;
}


const char* dcc2_qualify(const char* bla, double a, double b, double c, double d, double e, double f)
{
  entity one,two,three;
  vector<entity> conf;
  
  one.x = a;
  one.y = b;
  two.x = c;
  two.y = d;
  three.x = e;
  three.y = f;
  
  conf.push_back(one);
  conf.push_back(two);
  conf.push_back(three);
  std::string desc = writeQualitativeSceneTernary(FIRST2ALL,conf);
  //desc="4_0";
  const char* res = (char*)desc.c_str();
  if (strlen(res)>MAX_STRBUF) {
    sprintf(cres,"OBACHT!-Stringbuffer-zu-klein!");
  } else {
    for (unsigned int i=0; i<strlen(res); i++)
      cres[i] = res[i];
  }
  //    cerr << "CString: " << cres << endl;
  //    cerr << "CLen: " << strlen(cres) << endl;
  return cres;
}




// main

int main(int argc, char **argv) {
  // read command line arguments to get additional parameter for the calculus
  checkArgsNum(argc, 3, 
	       "The qualifier for this calculus requires the following parameters:\n- calculus parameter string (dcc2)\n- generation mode ('all' or 'first2all')\n"
  	       );
    
  if (strcmp(argv[1],"dcc2")!=0) {
    cerr << "Wrong number of parameters in calculus parameter string. "
	 << "Expected form: dcc2.\n";
    exit(2);
  }

  mode = readQualifierGenerationMode(argv[2]);

  // read scene description from stdin and parse it (see top of this file 
  // for input and output format)	
  std::vector<entity> confg = parseDescription();
	
  // generate qualitative description
  std::string desc = writeQualitativeSceneTernary(mode,confg);
	cout << desc << endl;

  return 0;	
}
