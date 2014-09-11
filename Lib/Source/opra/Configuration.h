/* This file is part of SparQ, a toolbox for qualitative spatial reasoning.
   Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
   More info at http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

  SparQ is free software and has been released under the terms of the GNU
  General Public License version 3 or later. You should have received a
  copy of the GNU General Public License along with this program. If not,
  see <http://www.gnu.org/licenses/>.
*/


#ifndef CONFIGURATION_H_
#define CONFIGURATION_H_

#include <iostream>

class Configuration {
public:

    Configuration(int _i, int _j, int _k, int _l, int _s, int _t);

    virtual ~Configuration();

    int i, j, k, l, s, t;



    int compareTo(Configuration c);

    friend std::ostream& operator<<(std::ostream &os, const Configuration &c) {
        os << c.i << " " << c.j << " " << c.k << " " << c.l << " " << c.s << " " << c.t;
        return os;
    }
};

#endif /*CONFIGURATION_H_*/
