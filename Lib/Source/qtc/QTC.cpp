/* This file is part of SparQ, a toolbox for qualitative spatial reasoning.
   Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
   More info at http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

  SparQ is free software and has been released under the terms of the GNU
  General Public License version 3 or later. You should have received a
  copy of the GNU General Public License along with this program. If not,
  see <http://www.gnu.org/licenses/>.
*/


#include <vector>
#include <string>
#include <iostream>
#include <set>
#include "Misc.h"
#include "QTC.h"

using namespace std;

string symbols[] = { "-", "0", "+" };

// upper part of the composition rules table
string crtUpper[][17] = {
  { "00", "00", "00", "00", "00", "00", "00", "00", "00", "00", "00",
    "00", "00", "00", "00", "00", "00" },
  { "-0", "-+", "-0", "+0", "--", "-+", "-+", "-+", "-+", "0+", "++",
    "+-", "0-", "--", "--", "--", "--" },
  { "--", "-*", "--", "++", "*-", "-*", "-*", "-*", "-*", "-+", "*+",
    "+*", "+-", "*-", "*-", "*-", "*-" },
  { "0-", "--", "0-", "0+", "+-", "--", "--", "--", "--", "-0", "-+",
    "++", "+0", "+-", "+-", "+-", "+-" },
  { "+-", "*-", "+-", "-+", "+*", "*-", "*-", "*-", "*-", "--", "-*",
    "*+", "++", "+*", "+*", "+*", "+*" },
  { "+0", "+-", "+0", "-0", "++", "+-", "+-", "+-", "+-", "0-", "--",
    "-+", "0+", "++", "++", "++", "++" },
  { "++", "+*", "++", "--", "*+", "+*", "+*", "+*", "+*", "+-", "*-",
    "-*", "-+", "*+", "*+", "*+", "*+" },
  { "0+", "++", "0+", "0-", "-+", "++", "++", "++", "++", "+0", "+-",
    "--", "-0", "-+", "-+", "-+", "-+" },
  { "-+", "*+", "-+", "+-", "-*", "*+", "*+", "*+", "*+", "++", "+*",
    "*-", "--", "-*", "-*", "-*", "-*" } };

// lower part of the composition rules table
string crtLower[][17] = {
  { "00", "00", "00", "00", "00", "00", "00", "00", "00", "00", "00",
    "00", "00", "00", "00", "00", "00" },
  { "+0", "++", "-0", "+0", "+-", "++", "-+", "0+", "++", "++", "++",
    "+-", "+-", "+-", "0-", "--", "+-" },
  { "++", "*+", "--", "++", "+*", "*+", "-*", "-+", "*+", "*+", "*+",
    "+*", "+*", "+*", "+-", "*-", "+*" },
  { "0+", "-+", "0-", "0+", "++", "-+", "--", "-0", "-+", "-+", "-+",
    "++", "++", "++", "+0", "+-", "++" },
  { "-+", "-*", "+-", "-+", "*+", "-*", "*-", "--", "-*", "-*", "-*",
    "*+", "*+", "*+", "++", "+*", "*+" },
  { "-0", "--", "+0", "-0", "-+", "--", "+-", "0-", "--", "--", "--",
    "-+", "-+", "-+", "0+", "++", "-+" },
  { "--", "*-", "++", "--", "-*", "*-", "+*", "+-", "*-", "*-", "*-",
    "-*", "-*", "-*", "-+", "*+", "-*" },
  { "0-", "+-", "0+", "0-", "--", "+-", "++", "+0", "+-", "+-", "+-",
    "--", "--", "--", "-0", "-+", "--" },
  { "+-", "+*", "-+", "+-", "*-", "+*", "*+", "++", "+*", "+*", "+*",
    "*-", "*-", "*-", "--", "-*", "*-" } };

string figure45[] = { "90-270", "180-270", "180-360", "90-180",                             // 0-3
		      "180", "180-270", "0-180", "90-180", "90-270", "90-180", "X",         // 4-10
		      "270-360", "90", "X", "270", "0-90", "X", "180-270", "0-180",         // 11-18
		      "0-90", "-90-90", "0-90", "0", "270-360", " -90-90", "270-360",       // 19-25
		      "180-360", "180-270", "270", "270-360", "X", "X", "X", "0-90",        // 26-33
		      "90", "90-180", "180", "X", "0", "X", "all", "X", "0", "X", "180",    // 34-44
		      "90-180", "90", "0-90", "X", "X", "X", "270-360", "270", "180-270",   // 45-53
		      "180-360", "270-360", "-90-90", "270-360", "0", "0-90", "-90-90",     // 54-60
		      "0-90", "0-180", "180-270", "X", "0-90", "270", "X", "90",            // 61-68
		      "270-360", "X", "90-180", "90-270", "90-180", "0-180", "180-270",     // 69-75
		      "180", "90-180", "180-360", "180-270", "90-270" };                    // 76-80

/**
 * returns corresponding row number in the upper CR-table for a given pair
 * of symbols.
 * 
 * @param s
 * @return
 */
int selectRowUpper(string s) {
  if (s == "00")
    return 0;
  else if (s == "-0")
    return 1;
  else if (s == "--")
    return 2;
  else if (s == "0-")
    return 3;
  else if (s == "+-")
    return 4;
  else if (s == "+0")
    return 5;
  else if (s == "++")
    return 6;
  else if (s == "0+")
    return 7;
  // Fall "-+"
  else
    return 8;
  
}

/**
 * returns corresponding row number in the lower CR-table for a given pair
 * of symbols.
 * 
 * @param s
 * @return
 */
int selectRowLower(string s) {
  if (s == "00")
    return 0;
  else if (s == "+0")
    return 1;
  else if (s == "++")
    return 2;
  else if (s == "0+")
    return 3;
  else if (s == "-+")
    return 4;
  else if (s == "-0")
    return 5;
  else if (s == "--")
    return 6;
  else if (s == "0-")
    return 7;
  // Fall "+-"
  else
    return 8;
}

/**
 * Select the column number of the composition rules table
 * 
 * @param i
 * @return
 */
int* selectColumn(string i)
{
  if (i == "90-270")
    {
      static int a[] = { 6, 7, 8, 9, 10, 2, 3, 11, 12, 13, 14, 15, -1 };
      int *p = a;
      return p;			
    }
  else if (i == "180-270")
    {
      static int a[] = { 11, 12, 13, 14, 15, -1 };
      int *p = a;
      return p;			
    }
  else if (i == "180-360")
    {
      static int a[] = { 11, 12, 13, 14, 15, 4, 16, -1 };
      int *p = a;
      return p;			
    }
  else if (i == "90-180")
    {
      static int a[] = { 6, 7, 8, 9, 10, -1 };
      int *p = a;
      return p;			
    }
  else if (i == "180")
    {
      static int a[] = { 2, 3, -1 };
      int *p = a;
      return p;			
    }
  else if (i == "X")
    {
      static int a[] = {-1};
      int *p = a;
      return p;			
    }
  else if (i == "270-360")
    {
      static int a[] = { 16, -1 };
      int *p = a;
      return p;			
    }
  else if (i == "90")
    {
      static int a[] = { 1, -1 };
      int *p = a;
      return p;			
    }
  else if (i == "0-180")
    {
      static int a[] = { 5, 1, 6, 7, 8, 9, 10, -1 };
      int *p = a;
      return p;			
    }
  else if (i == "0-90")
    {
      static int a[] = { 5, -1 };
      int *p = a;
      return p;			
    }
  else if (i == "-90-90")
    {
      static int a[] = { 0, 5, 16, -1 };
      int *p = a;
      return p;			
    }
  else if (i == "270")
    {
      static int a[] = { 4, -1 };
      int *p = a;
      return p;			
    }
  else if (i == "all")
    {
      static int a[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
			 14, 15, 16, -1 };
      int *p = a;
      return p;			
    }
  // Fall "0"
  else
    {
      static int a[] = { 0, -1 };
      int *p = a;
      return p;			
    }
}

/**
 * -, 0, + are converted to 0, 1, 2 respectively.
 * 
 * @param c
 * @return
 */
int toInt(char c) {
  if (c == '-')
    return 2;
  else if (c == '0')
    return 1;
  else
    return 0;
}

/**
 * For a given QTC relation this method searches for a matching number in
 * the figure 45
 * 
 * @param s
 * @return
 */
int findCellNr(string s) {
  return 27 * toInt(s[0]) + 9 * toInt(s[1]) + 3
    * toInt(s[2]) + toInt(s[3]) + 1;
}

/**
 * The core method for the programme.
 * 
 * @param r1
 * @param r2
 * @return
 */
std::set<string,Misc::conflt> lookUpTable(string r1, string r2) {
  
  std::set<string,Misc::conflt> comp;
  string tmpString = "";
  int rowU = selectRowUpper(tmpString + r1[0] + r1[2]);
  int rowL = selectRowLower(tmpString + r2[1] + r2[3]);
  
  // Beware -1 at the end of the array-index
  //printf("Figure 45: %s\n", figure45[findCellNr(tmpString + r1[1] + r2[0] + r1[3] + r2[2]) - 1].c_str());
  int *column = selectColumn(figure45[findCellNr(tmpString + r1[1] + r2[0] + r1[3] + r2[2]) - 1]);
  
  int i = 0;
  while (column[i] != -1)
    {
      // d1 d2 are the distance relations of the resulting relation and
      // o1, o2 are the orientation relations. The resulting relation
      // should therefore look like "d1 d2 o1 o2".
      string d_1 = "";
      string d_2 = "";
      string o_1 = "";
      string o_2 = "";
      
      d_1 = (crtUpper[rowU][column[i]])[0];
      o_1 = (crtUpper[rowU][column[i]])[1];
      d_2 = (crtLower[rowL][column[i]])[0];
      o_2 = (crtLower[rowL][column[i]])[1];
      
      vector<string> pool;
      
      // The symbol "*" is expanded to -, 0 and +
      if (d_1 == "*") {
	for (int k = 0; k < 3; k++) {
	  pool.push_back(symbols[k]);
	}
      } else
	pool.push_back(d_1);
      
      if (d_2 == "*") {
	vector<string> tmp;
	for (int k = 0; k < 3; k++) {
	  for (vector<string>::size_type k1 = 0; k1 < pool.size(); k1++)
	    tmp.push_back(pool[k1] + symbols[k]);
	}
	pool = tmp;
      } else {
	vector<string> tmp;
	for (vector<string>::size_type k1 = 0; k1 < pool.size(); k1++)
	  tmp.push_back(pool[k1] + d_2);
	pool = tmp;
      }
      
      if (o_1 == "*") {
	vector<string> tmp;
	for (int k = 0; k < 3; k++) {
	  for (vector<string>::size_type k1 = 0; k1 < pool.size(); k1++)
	    tmp.push_back(pool[k1] + symbols[k]);
	}
	pool = tmp;
      } else {
	vector<string> tmp;
	for (vector<string>::size_type k1 = 0; k1 < pool.size(); k1++)
	  tmp.push_back(pool[k1] + o_1);
	pool = tmp;
      }
      
      if (o_2 == "*") {
	vector<string> tmp;
	for (int k = 0; k < 3; k++) {
	  for (vector<string>::size_type k1 = 0; k1 < pool.size(); k1++)
	    tmp.push_back(pool[k1] + symbols[k]);
	}
	pool = tmp;
      } else {
	vector<string> tmp;
	for (vector<string>::size_type k1 = 0; k1 < pool.size(); k1++)
	  tmp.push_back(pool[k1] + o_2);
	pool = tmp;
      }
      
      for (vector<string>::size_type k = 0; k < pool.size(); k++) {
	comp.insert(pool[k]);
      }
      i++;
      //printf( "CT (column %i=\"%s\"): %s\n", column[i], printComp(comp).c_str());;
    }
  
  return comp;
}


string printComp(std::set<string,Misc::conflt> table)
{
  string comp = "";
  set<string,Misc::conflt>::iterator pos = table.begin();
  while (pos != table.end())
    {
      comp = comp + *pos + " ";
      pos++;
    }
  return comp;
}

string getQTCcomposition( string r1, string r2 ){
  return printComp(lookUpTable(r1,r2));
}

//Now defined in Interface
/*string qtc_c22_composition( string r1, string r2) {
  return lookUpTable( r1, r2 );
  }
  string qtc_c21_composition( string r1, string r2) {
  return lookUpTable( r1, r2 );
  }
*/

/*
int main( int argc, char ** argv ) {
  string r1, r2;
  if (argc==3) {
    r1 = argv[1];
    r2 = argv[2];
    cout << printComp(lookUpTable(argv[1], argv[2])) << endl;
  }
  return 0;
}
*/
