/* This file is part of SparQ, a toolbox for qualitative spatial reasoning.
   Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
   More info at http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

  SparQ is free software and has been released under the terms of the GNU
  General Public License version 3 or later. You should have received a
  copy of the GNU General Public License along with this program. If not,
  see <http://www.gnu.org/licenses/>.
*/


#ifndef __INTERFACE_H__
#define __INTERFACE_H__

#include "OpraOnTheFly.h"
#include <stdlib.h>
#include <string>

struct relation
{
	int i,j;
};

const char* return_result(std::string);
void parseRelation(const char*, struct relation*);

extern "C" const char* opra_converse(const char*, const char*);
extern "C" const char* opra_compose(const char*, const char*, const char*);
extern "C" const char* opra_qualify(const char*, double, double, double, double, double, double, double, double);

extern char cres[];

#endif
