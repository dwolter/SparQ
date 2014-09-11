/* This file is part of SparQ, a toolbox for qualitative spatial reasoning.
   Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
   More info at http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

  SparQ is free software and has been released under the terms of the GNU
  General Public License version 3 or later. You should have received a
  copy of the GNU General Public License along with this program. If not,
  see <http://www.gnu.org/licenses/>.
*/


// This class defines a certain relation for the container set containing configurations,
// so that elements in it may be automatically sorted.

#ifndef MISC_H_
#define MISC_H_

#include "Configuration.h"

class Misc {
public:
    Misc();
    virtual ~Misc();

    struct conflt {
        bool operator()(Configuration a, Configuration b) const {
            return (a.compareTo(b) < 0) ;
        }
    };

};

#endif /*MISC_H_*/
