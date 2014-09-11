/* This file is part of SparQ, a toolbox for qualitative spatial reasoning.
   Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
   More info at http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

  SparQ is free software and has been released under the terms of the GNU
  General Public License version 3 or later. You should have received a
  copy of the GNU General Public License along with this program. If not,
  see <http://www.gnu.org/licenses/>.
*/


#include "Configuration.h"

Configuration::Configuration(int _i, int _j, int _k, int _l, int _s, int _t) {
    i = _i;
    j = _j;
    k = _k;
    l = _l;
    s = _s;
    t = _t;
}

Configuration::~Configuration() {}

// defines relation on the set of configurations
int Configuration::compareTo(Configuration c) {

    int m = 1;

    if (i > c.i)
        return m;

    else if (i < c.i)
        return -m;

    else {
        if (j > c.j)
            return m;

        else if (j < c.j)
            return -m;

        else {
            if (k > c.k)
                return m;

            else if (k < c.k)
                return -m;

            else {
                if (l > c.l)
                    return m;

                else if (l < c.l)
                    return -m;

                else {
                    if (s > c.s)
                        return m;

                    else if (s < c.s)
                        return -m;

                    else {
                        if (t > c.t)
                            return m;

                        else if (t < c.t)
                            return -m;

                        else {
                            return 0;
                        }
                    }
                }
            }
        }
    }
}
