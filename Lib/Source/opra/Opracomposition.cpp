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

#include "OpraOnTheFly.h"
#include "Interface.h"

using namespace std;

int main( int argc, char ** argv ) {
	char		rel1[128];
	char		rel2[128];
    const char*	res;

    if (argc==6) {
		sprintf(rel1,"%s_%s", argv[2], argv[3]);
		sprintf(rel2,"%s_%s", argv[4], argv[5]);

        res = opra_compose(argv[1], rel1, rel2);
        cout << res << endl;
        cout << "String Length: " << strlen(res) << endl;
    }
}
