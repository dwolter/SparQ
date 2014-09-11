/* This file is part of SparQ, a toolbox for qualitative spatial reasoning.
   Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
   More info at http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

  SparQ is free software and has been released under the terms of the GNU
  General Public License version 3 or later. You should have received a
  copy of the GNU General Public License along with this program. If not,
  see <http://www.gnu.org/licenses/>.
*/


#include "Interface.h"

#include "qlbe_utils.h"
#include "opra_qlbe.h"
#include "qualifier.h"


/* Output buffer */
char cres[MAX_STRBUF];

/* Returns a std::string by copying it to the output buffer and returning a pointer to the buffer */
const char* return_result(std::string the_result)
{
	if (the_result.length() < MAX_STRBUF)
	{
		sprintf(cres,"%s",the_result.c_str());
	} else {
		sprintf(cres,"OBACHT!-Stringbuffer-zu-klein!");
	}
    return cres;
}

/* Parses a string (e.g. "7_1") and returns a relation struct {7,7} */
void parseRelation(const char* relation_name, struct relation* rel)
{
	sscanf(relation_name, "%i", &(rel->i));
	if (relation_name[0]=='s'||relation_name[0]=='S') /* Parsing S_... */
	{
		sscanf(relation_name+2, "%i", &(rel->i));
		rel->j = -1;
	} else {
		sscanf(index(relation_name,'_') +1, "%i", &(rel->j));
	}
	//cout << "* " << " "<< rel->i << " " << rel->j << endl;
}


/* OPRA composition operation */
const char* opra_compose(const char* param, const char* rel_ab_name, const char* rel_bc_name)
{
	struct relation	rel_ab;
	struct relation	rel_bc;
	int				n;
	
	/* Parse the granularity */
	n = atoi(param);
	
	/* Parse the string denoting a relation */
	parseRelation(rel_ab_name, &rel_ab);
	parseRelation(rel_bc_name, &rel_bc);
	
	/* Compute the composition */
    OpraOnTheFly newCmp(n, rel_ab.i, rel_ab.j, rel_bc.i, rel_bc.j);
    return return_result(newCmp.getComposition(newCmp.generateCompositionTable()));
	
}

/* OPRA converse operation */
const char* opra_converse(const char* param, const char* rel_name)
{
	int				n;
	struct relation	rel;
	
	/* Parse the granularity */
	n = atoi(param);

	/* Parse the relation */
	parseRelation(rel_name, &rel);

	/* Compute converse */
	return return_result(getConverse(n, rel.i, rel.j));
}


/* OPRA qualification */
const char* opra_qualify(const char* param, double x1, double y1, double dx1, double dy1, double x2, double y2, double dx2, double dy2)
{
	int				m;
	entity			one,two;
	vector<entity>	conf;
	
	/* Set the granularity in the qualification library*/
	m = atoi(param); 
	//setGranularity(m);
	
	/* Initialize the objects and stuff them into a vector (creating a mini-configuration) */
	// one.x = x1;
	// one.y = y1;
	// one.th = atan2(dy1,dx1);
	
	// two.x = x2;
	// two.y = y2;
	// two.th = atan2(dy2,dx2);
	
	 // conf.push_back(one);
	 // conf.push_back(two);

	/* Qualify the configuration */
	//return return_result( writeQualitativeSceneBinary(FIRST2ALL,conf) );

	return return_result(getOPRA_Relation_String(m,x1,y1,dx1,dy1,x2,y2,dx2,dy2));
}
