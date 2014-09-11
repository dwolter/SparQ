/* This file is part of SparQ, a toolbox for qualitative spatial reasoning.
   Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
   More info at http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

  SparQ is free software and has been released under the terms of the GNU
  General Public License version 3 or later. You should have received a
  copy of the GNU General Public License along with this program. If not,
  see <http://www.gnu.org/licenses/>.
*/

#include "qlbe_utils.h"
#include <cstdio>
#include <cstdlib>

using namespace std;

std::string s;
double d;

//std::vector<point> points;
//std::vector<opoint> opoints;
std::vector<entity> objects;

void parseError() {
	std::cerr << "Parse error.\n";
	exit(1);
}

int getToken() {
  char c;
	
  do {
    if (!std::cin) return ENDOFFILE;
    std::cin >> c;
  } while (isspace(c));
  
  switch (c) {
  case '(':	return LBRACKET;
  case ')':	return RBRACKET;			
  }	
	
  if (isdigit(c) || c == '-' || c == '+') {
    std::cin.putback(c);
    std::cin >> d;
    return NUMBER;	
  }
	
  if (isalnum(c)) {
    std::cin.putback(c);
    std::cin >> s;
    return ID;
  }
  
  return NOTATOKEN;
}

entity parsePoint() {
  entity o;
		
  if (getToken() != ID) parseError();
  o.s = s;
  if (getToken() != NUMBER) parseError();
  o.x = d;
  if (getToken() != NUMBER) parseError();
  o.y = d;
  if (getToken() != RBRACKET) parseError();

  objects.push_back(o);
	return o;
}

entity parseOPoint() {
  entity o;
  if (getToken() != ID) parseError();
  o.s = s;
  if (getToken() != NUMBER) parseError();
  o.x = d;
  if (getToken() != NUMBER) parseError();
  o.y = d;
  if (getToken() != NUMBER) parseError();
  o.th = d;
  if (getToken() != RBRACKET) parseError();
		
  return o;
}

entity parseDipole() {
  entity o;
		
  if (getToken() != ID) parseError();
  o.s = s;
  if (getToken() != NUMBER) parseError();
  o.sx = d;
  if (getToken() != NUMBER) parseError();
  o.sy = d;
  if (getToken() != NUMBER) parseError();
  o.ex = d;
  if (getToken() != NUMBER) parseError();
  o.ey = d;
  if (getToken() != RBRACKET) parseError();

  return o;
}

vector<entity> parseDescription() {
	vector<entity> res;
	entity o; entity p;
  if (getToken() != LBRACKET) parseError();
	
  int token;
  while ((token = getToken()) == LBRACKET) {
    o = parseObject(); 
//		cout << "...Noch da" << endl;
		res.push_back(o);
  }
  if (token != RBRACKET) parseError();
//  cout << "Noch da" << endl;
  return res;
}

string writeQualitativeSceneBinary(int mode, vector<entity> objects) {
	string res = ""; // "( ";
  if (mode == ALL) {
    for (unsigned int i = 0; i < objects.size(); i++) {
      for (unsigned int j = i+1; j < objects.size(); j++) {
				res += "(" + objects[i].s + " " + getRelation(objects[i], objects[j])
					+ " " + objects[j].s + ") ";
 			}
    }
  }
  if (mode == FIRST2ALL) {
    for (unsigned int j = 1; j < objects.size(); j++) {
			res += "(" + objects[0].s + " " + getRelation(objects[0], objects[j])
				+ " " + objects[j].s + ") ";
		}
	}
//	res += ")";
	return res;
}
			
  //~ std::cout << "( ";
  //~ if (mode == ALL) {
    //~ for (unsigned int i = 0; i < objects.size(); i++) {
      //~ for (unsigned int j = i+1; j < objects.size(); j++) {
	//~ std::cout << "(" << (objects[i].s) << " " << getRelation(objects[i], 
								 //~ objects[j])
		  //~ << " " << (objects[j].s) << ") ";
      //~ }
    //~ }
  //~ }
  //~ if (mode == FIRST2ALL) {
    //~ for (unsigned int j = 1; j < objects.size(); j++) {
      //~ std::cout << "(" << (objects[0].s) << " " 
		//~ << getRelation(objects[0], 
			       //~ objects[j])
		//~ << " " << (objects[j].s) << ") ";
    //~ }
  //~ }
  
  //~ std::cout << ")\n";			
//~ }

string writeQualitativeSceneTernary(int mode, vector<entity> objects) {
	string res;
  if (mode == ALL) {
    for (unsigned int i = 0; i < objects.size(); i++) {
      for (unsigned int j = i+1; j < objects.size(); j++) {
	      for (unsigned int k = j+1; k < objects.size(); k++) {
	        res+= "(" + objects[i].s + " " 
	       		+ objects[j].s + " "
	       		+ getRelation(objects[i],objects[j],objects[k])
	       + " " + objects[k].s + ") ";
				}
      }
    }
  }
	if (mode == FIRST2ALL) {
    for (unsigned int j = 1; j < objects.size(); j++) {
      for (unsigned int k = j+1; k < objects.size(); k++) {
				res += "(" + objects[0].s + " " + objects[j].s + " "
	     		+ getRelation(objects[0], objects[j], objects[k])
	     		+ " " + objects[k].s + ") ";
      }
    }
  }
return res;	
}


  //~ std::cout << "( ";
  //~ if (mode == ALL) {
    //~ for (unsigned int i = 0; i < objects.size(); i++) {
      //~ for (unsigned int j = i+1; j < objects.size(); j++) {
	//~ for (unsigned int k = j+1; k < objects.size(); k++) {
	  //~ std::cout << "(" << objects[i].s << " " 
	       //~ << objects[j].s << " "
	       //~ << getRelation(objects[i],
			      //~ objects[j],
			      //~ objects[k])
	       //~ << " " << objects[k].s << ") ";
	//~ }
      //~ }
    //~ }
  //~ }
  //~ if (mode == FIRST2ALL) {
    //~ for (unsigned int j = 1; j < objects.size(); j++) {
      //~ for (unsigned int k = j+1; k < objects.size(); k++) {
	//~ std::cout << "(" << objects[0].s << " " 
	     //~ << objects[j].s << " "
	     //~ << getRelation(objects[0],
			    //~ objects[j],
			    //~ objects[k])
	     //~ << " " << objects[k].s << ") ";
      //~ }
    //~ }
  //~ }	
  //~ std::cout << ")\n";			


int readQualifierGenerationMode(char* arg) {
  int mode;
	
  if (strcmp(arg,"all") == 0)
    mode = ALL;
  else if (strcmp(arg,"first2all") == 0)
    mode = FIRST2ALL;
  else {
    std::cerr << "Wrong argument. The generation modus needs to be"   
	 << " specified (valid arguments are 'all' and"
	 << " 'first2all').\n";
    exit(1);
  }

  return mode;
}

void checkArgsNum(int argc, int reqParams, std::string helpText) {
  if (argc != reqParams) {
    std::cerr << "Wrong number of arguments. " << helpText << "\n";
    exit(1);
  }
}

void checkTokenNum(std::vector<char*> tokens, int reqTokens, 
		   std::string helpText) {
  if (tokens.size() != reqTokens) {
    std::cerr << "Wrong number of parameters in calculus parameter string. " 
	      << helpText << "\n";
    exit(1);
  }
}

void checkStringArg(char* arg, char* str, std::string errorText) {
  if (strcmp(arg,str) != 0) {
    std::cerr << errorText << "\n";
    exit(1);
  }
}

int readIntArg(char* arg, std::string errorText) {
  int i;
  
  if (sscanf(arg, "%d", &i) == 0) {
    std::cerr << errorText << "\n";
    exit(1);
  }

  return i;
}

bool readBoolArg(char* arg, std::string errorText) {
  bool b;
	
  if (strcmp(arg,"true") == 0) 
    b = true;
  else if (strcmp(arg,"false") == 0) 
    b = false;
  else {
    std::cerr << errorText << "\n";
    exit(1);
  }

  return b;
}

double readDoubleArg(char* arg, std::string errorText) {
  double d;
	
  if (sscanf(arg, "%lf", &d) == 0) {
    std::cerr << errorText << "\n";
    exit(1);
  }

  return d;
}

float readFloatArg(char* arg, std::string errorText) {
  float f;
	
  if (sscanf(arg, "%f", &f) == 0) {
    std::cerr << errorText << "\n";
    exit(1);
  }

  return f;
}

vector<char*> splitString(char* str, const char* delim) {
  vector<char*> results;
  char* next = strtok(str, delim);
  
  while (next != NULL) {
    results.push_back(next);
    next = strtok(NULL, delim);
  }

  return results;
}
