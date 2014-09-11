/* This file is part of SparQ, a toolbox for qualitative spatial reasoning.
   Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
   More info at http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

  SparQ is free software and has been released under the terms of the GNU
  General Public License version 3 or later. You should have received a
  copy of the GNU General Public License along with this program. If not,
  see <http://www.gnu.org/licenses/>.
*/


/** 
 * Qualifier backend for opra. 
 * input format: ( (id1 x1 y1 o1) (id2 x2 y2 o2) (id3 x3 y3 o3) ... )
 * output format: ( (id1 rel id2) (id1 rel id3) ... )
 */

#include "qlbe_utils.h"
#include "qualifier.h"
	
#include "opra_qlbe.h"

int mode;
int gran;

entity parseObject() {
  return parseOPoint();
}

void setGranularity(int m)
{
	gran = m;
}

std::string getRelation(const entity& o1, const entity& o2) {
  return getOPRA_Relation_String(gran, o1.x, o1.y, o1.th, 
				 o2.x, o2.y, o2.th);
}

std::string getRelation(const entity& o1, const entity& o2, const entity& o3) {
  return "";
}

// main

int main(int argc, char **argv) {
  // read command line arguments to get additional parameter for the calculus
  checkArgsNum(argc, 3, 
	       "The qualifier for this calculus requires the following parameters:\n- calculus parameter string (opra-<granularity>)\n- generation mode ('all' or 'first2all')\n"
	       );

  vector<char*> tokens = splitString(argv[1],"-");
  
  checkTokenNum(tokens, 2,
		"The expected form is opra-<granularity>.\n"	
		);

  gran = readIntArg(tokens[1],
		    "Wrong argument. The granularity is required as a positive integer argument."
		    );
	cout << "Gran: " << gran << endl;
  mode = readQualifierGenerationMode(argv[2]);
	cout << "Noch da" << endl;
  // read scene description from stdin and parse it (see top of this file 
  // for input and output format)	
  std::vector<entity> confg = parseDescription();
	cout << "immer noch da" << endl;
  // generate qualitative description
  std::string desc = writeQualitativeSceneBinary(mode,confg);
	cout << desc << endl;

return 0;	
}
