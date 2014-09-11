/* This file is part of SparQ, a toolbox for qualitative spatial reasoning.
   Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
   More info at http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

  SparQ is free software and has been released under the terms of the GNU
  General Public License version 3 or later. You should have received a
  copy of the GNU General Public License along with this program. If not,
  see <http://www.gnu.org/licenses/>.
*/

#ifndef __QLBE_UTILS__
#define __QLBE_UTILS__

#include <iostream>
#include <vector>
#include <cctype>
#include <cmath>
#include <cstring>

using namespace std;

// basic entities

class entity {
 public:
  std::string s;
 
  double x;
  double y;
  double th;

  double sx;
  double sy;
  double ex;
  double ey;
 
 	void operator=(const entity a) { 
		x = a.x; y = a.y; th = a.th;
		sx = a.sx; sy = a.sy; ex = a.ex; ey = a.ey;
		};
};

// external functions expected by the specific qualifier backend

extern entity parseObject(); // should call either parsePoint, parseOPoint, 
                           // or parseDipole depending on the calculus

extern std::string getRelation(const entity& o1, const entity& o2);
extern std::string getRelation(const entity& o1, const entity& o2,
			       const entity& o3);


// parser

const int NOTATOKEN = -2;
const int ENDOFFILE = -1;
const int LBRACKET = 0;
const int RBRACKET = 1;
const int ID = 2;
const int NUMBER = 3;

entity parsePoint();
entity parseOPoint();
entity parseDipole();
std::vector<entity> parseDescription();
	
// qualifier specific stuff

//extern std::vector<entity> objects;

const int ALL = 0;
const int FIRST2ALL = 1;	

int readQualifierGenerationMode(char* arg);
std::string writeQualitativeSceneBinary(int mode,std::vector<entity> objects);
std::string writeQualitativeSceneTernary(int mode,std::vector<entity> objects);

// general functions to parse args

void checkArgsNum(int argc, int reqParams, std::string helptext);
void checkTokenNum(std::vector<char*> tokens, int reqTokens, 
		   std::string helpText);
void checkStringArg(char* arg, char* str, std::string errorText);
int readIntArg(char* arg, std::string errorText);
bool readBoolArg(char* arg, std::string errorText);
double readDoubleArg(char* arg, std::string errorText);
float readFloatArg(char* arg, std::string errorText);

// general utility functions

std::vector<char*> splitString(char* str, const char* delim);

#endif
