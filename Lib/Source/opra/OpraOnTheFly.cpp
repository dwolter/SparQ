/* This file is part of SparQ, a toolbox for qualitative spatial reasoning.
   Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
   More info at http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

  SparQ is free software and has been released under the terms of the GNU
  General Public License version 3 or later. You should have received a
  copy of the GNU General Public License along with this program. If not,
  see <http://www.gnu.org/licenses/>.
*/


// This code is the core of the OPRA algorithm. To understand it well it is recommended
// to read the function generateCompositionTable() at first.

#include "OpraOnTheFly.h"
#include <iostream>
#include <vector>
#include <cstdio>

using namespace std;

// granularity for a certain problem
int m;


OpraOnTheFly::OpraOnTheFly(int _n, int _i, int _j, int _k, int _l) {
    m = _n;
    OpraOnTheFly::i = _i;
    OpraOnTheFly::j = _j;
    OpraOnTheFly::k = _k;
    OpraOnTheFly::l = _l;

}

OpraOnTheFly::~OpraOnTheFly() {}

// function for calculating modulo operation
int mod(int a, int b) {

    int c = a % b;
    return c >= 0 ? c : c + b;

}

// returns x mod 4*m
int mod4m(int a) {

    return mod(a, 4 * m);

}

// returns x mod 2
int mod2(int a) {

    return mod(a, 2);

}

//
vector< vector<int> > getZeros(vector<int> c) {
    vector< vector<int> > v;

    if ((mod2(c[0]) == 0 && mod2(c[1]) == 0)) {
        vector<int> tmp(2);
        tmp[0] = c[0];
        tmp[1] = c[1];
        v.push_back(tmp);

    }

    if ((mod2(c[2]) == 0 && mod2(c[3]) == 0)) {
        vector<int> tmp(2);
        tmp[0] = c[2];
        tmp[1] = c[3];
        v.push_back(tmp);

    }

    if ((mod2(c[4]) == 0 && mod2(c[5]) == 0)) {
        vector<int> tmp(2);
        tmp[0] = c[4];
        tmp[1] = c[5];
        v.push_back(tmp);

    }

    return v;
}

vector< vector<int> > getUnits(vector<int> c) {
    vector< vector<int> > v;

    if ((mod2(c[0]) == 1 && mod2(c[1]) == 0)
            || (mod2(c[0]) == 0 && mod2(c[1]) == 1)) {
        vector<int> tmp(2);
        tmp[0] = c[0];
        tmp[1] = c[1];
        v.push_back(tmp);

    }

    if ((mod2(c[2]) == 1 && mod2(c[3]) == 0)
            || (mod2(c[2]) == 0 && mod2(c[3]) == 1)) {
        vector<int> tmp(2);
        tmp[0] = c[2];
        tmp[1] = c[3];
        v.push_back(tmp);

    }

    if ((mod2(c[4]) == 1 && mod2(c[5]) == 0)
            || (mod2(c[4]) == 0 && mod2(c[5]) == 1)) {
        vector<int> tmp(2);
        tmp[0] = c[4];
        tmp[1] = c[5];
        v.push_back(tmp);

    }

    return v;
}

vector< vector<int> > getOnes(vector<int> c) {
    vector< vector<int> > v;

    if ((mod2(c[0]) == 1 && mod2(c[1]) == 1)) {
        vector<int> tmp(2);
        tmp[0] = c[0];
        tmp[1] = c[1];
        v.push_back(tmp);

    }

    if ((mod2(c[2]) == 1 && mod2(c[3]) == 1)) {
        vector<int> tmp(2);
        tmp[0] = c[2];
        tmp[1] = c[3];
        v.push_back(tmp);

    }

    if ((mod2(c[4]) == 1 && mod2(c[5]) == 1)) {
        vector<int> tmp(2);
        tmp[0] = c[4];
        tmp[1] = c[5];
        v.push_back(tmp);

    }

    return v;
}

// classifies the given configuration according to its construction.
// For example, if a configuration has two zerovectors and one unitvector then
// it belongs to the 2nd class.
int classifyConfiguration(vector<int> configClass) {

    int c = 0;

    if (configClass[0] == 3 && configClass[1] == 0 && configClass[2] == 0)
        c = 1;

    if (configClass[0] == 2 && configClass[1] == 1 && configClass[2] == 0)
        c = 2;

    if (configClass[0] == 1 && configClass[1] == 2 && configClass[2] == 0)
        c = 3;

    if (configClass[0] == 0 && configClass[1] == 3 && configClass[2] == 0)
        c = 4;

    if (configClass[0] == 2 && configClass[1] == 0 && configClass[2] == 1)
        c = 5;

    if (configClass[0] == 1 && configClass[1] == 1 && configClass[2] == 1)
        c = 6;

    if (configClass[0] == 0 && configClass[1] == 2 && configClass[2] == 1)
        c = 7;

    if (configClass[0] == 1 && configClass[1] == 0 && configClass[2] == 2)
        c = 8;

    if (configClass[0] == 0 && configClass[1] == 1 && configClass[2] == 2)
        c = 9;

    if (configClass[0] == 0 && configClass[1] == 0 && configClass[2] == 3)
        c = 10;

    return c;

}

// this function generate compositions
set<Configuration,Misc::conflt> OpraOnTheFly::generateCompositionTable() {


    // generate OPRA-compostions with fixed i, j, s, t and varying s, t
    // and proof afterwards if they are valid.
    for (int s = 0; s < 4 * m; s++)
        for (int t = -1; t < 4 * m; t++) {

            // begin of non-same-case
            if (j != -1 && l != -1 && t != -1) {

                // create an OPRA configuration
                vector<int> configuration(6);

                configuration[0] = t;
                configuration[1] = i;
                configuration[2] = j;
                configuration[3] = k;
                configuration[4] = l;
                configuration[5] = s;

                // generate containers that contain either zerovectors, unitvectors or one-vectors
                vector< vector<int> >  zeros = getZeros(configuration);
                vector< vector<int> > units = getUnits(configuration);
                vector< vector<int> > ones = getOnes(configuration);

                // create a vector in which the number of zero-, unit- and one-vectors are saved.
                vector<int> configClass(3);
                configClass[0] = zeros.size();
                configClass[1] = units.size();
                configClass[2] = ones.size();



                switch (classifyConfiguration(configClass)) {

                case 1: {
                    if ((mod4m(t - i) != 0) && (mod4m(j - k) != 0)
                            && (mod4m(l - s) != 0)) {

                        // Creating the set K1'
                        if (mod4m(t - i) + mod4m(j - k) + mod4m(l - s) == 2 * m) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);

                        }

                        // Creating the set K1~
                        if (mod4m(i - t) + mod4m(k - j) + mod4m(s - l) == 2 * m) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }
                    } else {

                        // Creating the set K1'' for
                        // degenerated
                        // Triangle Konfiguration
                        if ((mod4m(t - i) == 0 && mod4m(j - k) == 0 && mod4m(l
                                - s) == 2 * m)
                                || (mod4m(t - i) == 0 && mod4m(j - k) == 2 * m && mod4m(l
                                        - s) == 0)
                                || (mod4m(t - i) == 2 * m && mod4m(j - k) == 0 && mod4m(l
                                        - s) == 0)) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }
                    }

                    break;

                }

                case 3: {


                    if (mod4m((zeros[0])[0]
                              - ((zeros[0])[1])) != 0) {

                        // Creating the set K3
                        if (mod4m(t - i) + mod4m(j - k) + mod4m(l - s) == 2 * m) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                        // Creating the set K3~
                        if (mod4m(i - t) + mod4m(k - j) + mod4m(s - l) == 2 * m) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                    }

                    break;
                }

                case 4: {

                    // Creating the set K4
                    if ((mod4m(t - i) + mod4m(j - k) + mod4m(l - s) == 2 * m - 1)
                            || (mod4m(t - i) + mod4m(j - k) + mod4m(l - s) == 2 * m + 1)) {

                        Configuration tmp(i,j,k,l,s,t);
                        compositionTable.insert(tmp);


                    }
                    // Creating the set K4~
                    if ((mod4m(i - t) + mod4m(k - j) + mod4m(s - l) == 2 * m - 1)
                            || (mod4m(i - t) + mod4m(k - j) + mod4m(s - l) == 2 * m + 1)) {

                        Configuration tmp(i,j,k,l,s,t);
                        compositionTable.insert(tmp);


                    }

                    break;
                }

                case 5: {
                    if ((mod4m(t - i) != 0) && (mod4m(j - k) != 0)
                            && (mod4m(l - s) != 0)) {

                        // Creating the set K5'
                        if (mod4m(t - i) + mod4m(j - k) + mod4m(l - s) == 2 * m) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                        // Creating the set K5~
                        if (mod4m(i - t) + mod4m(k - j) + mod4m(s - l) == 2 * m) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }
                    } else {

                        // Creating the set K5'' for
                        // degenerated
                        // Triangle Konfiguration
                        if ((mod4m(t - i) == 0 && mod4m(j - k) == 0 && mod4m(l
                                - s) == 2 * m)
                                || (mod4m(t - i) == 0 && mod4m(j - k) == 2 * m && mod4m(l
                                        - s) == 0)
                                || (mod4m(t - i) == 2 * m && mod4m(j - k) == 0 && mod4m(l
                                        - s) == 0)) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }
                    }

                    break;
                }

                case 6: {

                    if ((mod4m(t - i) != 0) && (mod4m(j - k) != 0)
                            && (mod4m(l - s) != 0)) {

                        // Creating the set K6'
                        if ((mod4m(t - i) + mod4m(j - k) + mod4m(l - s) == 2 * m - 1)
                                || (mod4m(t - i) + mod4m(j - k) + mod4m(l - s) == 2 * m + 1)) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                        // Creating the set K6'~
                        if ((mod4m(i - t) + mod4m(k - j) + mod4m(s - l) == 2 * m - 1)
                                || (mod4m(i - t) + mod4m(k - j) + mod4m(s - l) == 2 * m + 1)) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }
                    } else if ((mod4m(zeros[0][0]
                                      - zeros[0][1]) != 0)
                               && (mod4m(ones[0][0]
                                         - ones[0][1]) == 0)) {

                        // Creating the set K6''
                        if (mod4m(t - i) + mod4m(j - k) + mod4m(l - s) == 2 * m - 1) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                        // Creating the set K6''~
                        if (mod4m(i - t) + mod4m(k - j) + mod4m(s - l) == 2 * m - 1) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                    }

                    break;
                }

                case 7: {

                    if ((mod4m(ones[0][0]
                               - ones[0][1]) != 0)) {

                        // Creating the set K7'
                        if ((mod4m(t - i) + mod4m(j - k) + mod4m(l - s) == 2 * m - 2)
                                || (mod4m(t - i) + mod4m(j - k) + mod4m(l - s) == 2 * m)
                                || (mod4m(t - i) + mod4m(j - k) + mod4m(l - s) == 2 * m + 2)) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                        // Creating the set K7'~
                        if ((mod4m(i - t) + mod4m(k - j) + mod4m(s - l) == 2 * m - 2)
                                || (mod4m(i - t) + mod4m(k - j) + mod4m(s - l) == 2 * m)
                                || (mod4m(i - t) + mod4m(k - j) + mod4m(s - l) == 2 * m + 2)) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                    } else {

                        // Creating the set K7''
                        if ((mod4m(t - i) + mod4m(j - k) + mod4m(l - s) == 2 * m - 2)
                                || (mod4m(t - i) + mod4m(j - k) + mod4m(l - s) == 2 * m)) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                        // Creating the set K7''~
                        if ((mod4m(i - t) + mod4m(k - j) + mod4m(s - l) == 2 * m - 2)
                                || (mod4m(i - t) + mod4m(k - j) + mod4m(s - l) == 2 * m)) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                    }
                    break;
                }

                case 8: {

                    // Creating the set K8' for degenerted
                    // triangle configurations
                    if ((mod4m(zeros[0][0]
                               - zeros[0][1]) == 0)) {
                        if (((mod4m(ones[0][0]
                                    - ones[0][1]) == 2 * m) && (mod4m(ones[1][0]
                                                                      - ones[1][1]) == 0))
                                || ((mod4m(ones[0][0]
                                           - ones[0][1]) == 0) && (mod4m(ones[1][0]
                                                                         - ones[1][1]) == 2 * m))) {
                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }
                    }

                    // Creating the set K8''
                    else if ((mod4m(zeros[0][0]
                                    - zeros[0][1]) != 0)
                             && (mod4m(ones[0][0]
                                       - ones[0][1]) == 0)
                             && (mod4m(ones[1][0]
                                       - ones[1][1]) == 0)) {
                        if ((mod4m(zeros[0][0]
                                   - zeros[0][1]) == 2 * m)
                                || (mod4m(zeros[0][0]
                                          - zeros[0][1]) == 2 * m - 2)) {
                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }
                        // Creating the set K8''~
                        if ((mod4m(zeros[0][1]
                                   - zeros[0][0]) == 2 * m)
                                || (mod4m(zeros[0][1]
                                          - zeros[0][0]) == 2 * m - 2)) {
                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }
                    }

                    // Creating the set K8'''

                    else if ((mod4m(zeros[0][0]
                                    - zeros[0][1]) != 0)
                             && (mod4m(ones[0][0]
                                       - ones[0][1]) == 0)
                             && (mod4m(ones[1][0]
                                       - ones[1][1]) != 0)
                             || (mod4m(ones[0][0]
                                       - ones[0][1]) != 0)
                             && (mod4m(ones[1][0]
                                       - ones[1][1]) == 0)) {

                        if ((mod4m(zeros[0][0]
                                   - zeros[0][1])
                                + mod4m(ones[0][0]
                                        - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1]) == 2 * m)
                                || (mod4m(zeros[0][0]
                                          - zeros[0][1])
                                    + mod4m(ones[0][0]
                                            - ones[0][1])
                                    + mod4m(ones[1][0]
                                            - ones[1][1]) == 2 * m - 2)) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                        // Creating the set K8'''~
                        if ((mod4m(zeros[0][1]
                                   - zeros[0][0])
                                + mod4m(ones[0][1]
                                        - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0]) == 2 * m)
                                || (mod4m(zeros[0][1]
                                          - zeros[0][0])
                                    + mod4m(ones[0][1]
                                            - ones[0][0])
                                    + mod4m(ones[1][1]
                                            - ones[1][0]) == 2 * m - 2)) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                    }

                    // Creating the set K8''''
                    else if ((mod4m(zeros[0][0]
                                    - zeros[0][1]) != 0)
                             && (mod4m(ones[0][0]
                                       - ones[0][1]) != 0)
                             && (mod4m(ones[1][0]
                                       - ones[1][1]) != 0)) {

                        if ((mod4m(zeros[0][0]
                                   - zeros[0][1])
                                + mod4m(ones[0][0]
                                        - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1]) == 2 * m - 2)
                                || (mod4m(zeros[0][0]
                                          - zeros[0][1])
                                    + mod4m(ones[0][0]
                                            - ones[0][1])
                                    + mod4m(ones[1][0]
                                            - ones[1][1]) == 2 * m)
                                || (mod4m(zeros[0][0]
                                          - zeros[0][1])
                                    + mod4m(ones[0][0]
                                            - ones[0][1])
                                    + mod4m(ones[1][0]
                                            - ones[1][1]) == 2 * m + 2)) {
                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }
                        // Creating the set K8''''~
                        if ((mod4m(zeros[0][1]
                                   - zeros[0][0])
                                + mod4m(ones[0][1]
                                        - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0]) == 2 * m - 2)
                                || (mod4m(zeros[0][1]
                                          - zeros[0][0])
                                    + mod4m(ones[0][1]
                                            - ones[0][0])
                                    + mod4m(ones[1][1]
                                            - ones[1][0]) == 2 * m)
                                || (mod4m(zeros[0][1]
                                          - zeros[0][0])
                                    + mod4m(ones[0][1]
                                            - ones[0][0])
                                    + mod4m(ones[1][1]
                                            - ones[1][0]) == 2 * m + 2)) {
                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                    }

                    break;

                }

                case 9: {

                    if (mod4m(ones[0][0]
                              - ones[0][1]) == 0
                            && mod4m(ones[1][0]
                                     - ones[1][1]) == 0) {

                        // Creating the set K9'
                        if (mod4m(units[0][0]
                                  - units[0][1]) == 2 * m - 3
                                || mod4m(units[0][0]
                                         - units[0][1]) == 2 * m - 1) {
                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                        // Creating the set K9'~
                        if (mod4m(units[0][1]
                                  - units[0][0]) == 2 * m - 3
                                || mod4m(units[0][1]
                                         - units[0][0]) == 2 * m - 1) {
                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                    }

                    if ((mod4m(ones[0][0]
                               - ones[0][1]) == 0 && mod4m(ones[1][0]
                                                           - ones[1][1]) != 0)
                            || (mod4m(ones[0][0]
                                      - ones[0][1]) != 0 && mod4m(ones[1][0]
                                                                  - ones[1][1]) == 0)) {

                        // Creating the set K9''
                        if (mod4m(units[0][0]
                                  - units[0][1])
                                + mod4m(ones[0][0]
                                        - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1]) == 2 * m - 3
                                || mod4m(units[0][0]
                                         - units[0][1])
                                + mod4m(ones[0][0]
                                        - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1]) == 2 * m - 1
                                || mod4m(units[0][0]
                                         - units[0][1])
                                + mod4m(ones[0][0]
                                        - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1]) == 2 * m + 1) {
                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                        // Creating the set K9''~
                        if (mod4m(units[0][1]
                                  - units[0][0])
                                + mod4m(ones[0][1]
                                        - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0]) == 2 * m - 3
                                || mod4m(units[0][1]
                                         - units[0][0])
                                + mod4m(ones[0][1]
                                        - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0]) == 2 * m - 1
                                || mod4m(units[0][1]
                                         - units[0][0])
                                + mod4m(ones[0][1]
                                        - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0]) == 2 * m + 1) {
                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                    } else {

                        // Creating the set K9'''
                        if (mod4m(units[0][0]
                                  - units[0][1])
                                + mod4m(ones[0][0]
                                        - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1]) == 2 * m - 3
                                || mod4m(units[0][0]
                                         - units[0][1])
                                + mod4m(ones[0][0]
                                        - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1]) == 2 * m - 1
                                || mod4m(units[0][0]
                                         - units[0][1])
                                + mod4m(ones[0][0]
                                        - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1]) == 2 * m + 1
                                || mod4m(units[0][0]
                                         - units[0][1])
                                + mod4m(ones[0][0]
                                        - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1]) == 2 * m + 3) {
                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                        // Creating the set K9'''~
                        if (mod4m(units[0][1]
                                  - units[0][0])
                                + mod4m(ones[0][1]
                                        - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0]) == 2 * m - 3
                                || mod4m(units[0][1]
                                         - units[0][0])
                                + mod4m(ones[0][1]
                                        - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0]) == 2 * m - 1
                                || mod4m(units[0][1]
                                         - units[0][0])
                                + mod4m(ones[0][1]
                                        - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0]) == 2 * m + 1
                                || mod4m(units[0][1]
                                         - units[0][0])
                                + mod4m(ones[0][1]
                                        - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0]) == 2 * m + 3) {
                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                    }

                    break;
                }

                case 10: {

                    // Creating the set K10'
                    if (mod4m(ones[0][0]
                              - ones[0][1]) != 0
                            && mod4m(ones[1][0]
                                     - ones[1][1]) != 0
                            && mod4m(ones[2][0]
                                     - ones[2][1]) != 0) {

                        if (mod4m(ones[0][0]
                                  - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1])
                                + mod4m(ones[2][0]
                                        - ones[2][1]) == 2 * m - 4
                                || mod4m(ones[0][0]
                                         - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1])
                                + mod4m(ones[2][0]
                                        - ones[2][1]) == 2 * m - 2
                                || mod4m(ones[0][0]
                                         - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1])
                                + mod4m(ones[2][0]
                                        - ones[2][1]) == 2 * m
                                || mod4m(ones[0][0]
                                         - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1])
                                + mod4m(ones[2][0]
                                        - ones[2][1]) == 2 * m + 2
                                || mod4m(ones[0][0]
                                         - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1])
                                + mod4m(ones[2][0]
                                        - ones[2][1]) == 2 * m + 4) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                        // Creating the set K10'~
                        if (mod4m(ones[0][1]
                                  - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0])
                                + mod4m(ones[2][1]
                                        - ones[2][0]) == 2 * m - 4
                                || mod4m(ones[0][1]
                                         - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0])
                                + mod4m(ones[2][1]
                                        - ones[2][0]) == 2 * m - 2
                                || mod4m(ones[0][1]
                                         - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0])
                                + mod4m(ones[2][1]
                                        - ones[2][0]) == 2 * m
                                || mod4m(ones[0][1]
                                         - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0])
                                + mod4m(ones[2][1]
                                        - ones[2][0]) == 2 * m + 2
                                || mod4m(ones[0][1]
                                         - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0])
                                + mod4m(ones[2][1]
                                        - ones[2][0]) == 2 * m + 4) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                    }
                    // Creating the set K10''
                    else if ((mod4m(ones[0][0]
                                    - ones[0][1]) != 0
                              && mod4m(ones[1][0]
                                       - ones[1][1]) != 0 && mod4m(ones[2][0]
                                                                   - ones[2][1]) == 0)
                             || (mod4m(ones[0][0]
                                       - ones[0][1]) != 0
                                 && mod4m(ones[1][0]
                                          - ones[1][1]) == 0 && mod4m(ones[2][0]
                                                                      - ones[2][1]) != 0)
                             || (mod4m(ones[0][0]
                                       - ones[0][1]) == 0
                                 && mod4m(ones[1][0]
                                          - ones[1][1]) != 0 && mod4m(ones[2][0]
                                                                      - ones[2][1]) != 0)) {
                        if (mod4m(ones[0][0]
                                  - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1])
                                + mod4m(ones[2][0]
                                        - ones[2][1]) == 2 * m - 4
                                || mod4m(ones[0][0]
                                         - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1])
                                + mod4m(ones[2][0]
                                        - ones[2][1]) == 2 * m - 2
                                || mod4m(ones[0][0]
                                         - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1])
                                + mod4m(ones[2][0]
                                        - ones[2][1]) == 2 * m
                                || mod4m(ones[0][0]
                                         - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1])
                                + mod4m(ones[2][0]
                                        - ones[2][1]) == 2 * m + 2) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                        // Creating the set K10''~
                        if (mod4m(ones[0][1]
                                  - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0])
                                + mod4m(ones[2][1]
                                        - ones[2][0]) == 2 * m - 4
                                || mod4m(ones[0][1]
                                         - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0])
                                + mod4m(ones[2][1]
                                        - ones[2][0]) == 2 * m - 2
                                || mod4m(ones[0][1]
                                         - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0])
                                + mod4m(ones[2][1]
                                        - ones[2][0]) == 2 * m
                                || mod4m(ones[0][1]
                                         - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0])
                                + mod4m(ones[2][1]
                                        - ones[2][0]) == 2 * m + 2) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                    }

                    // Creating the set K10'''
                    else if ((mod4m(ones[0][0]
                                    - ones[0][1]) == 0
                              && mod4m(ones[1][0]
                                       - ones[1][1]) == 0 && mod4m(ones[2][0]
                                                                   - ones[2][1]) != 0)
                             || (mod4m(ones[0][0]
                                       - ones[0][1]) == 0
                                 && mod4m(ones[1][0]
                                          - ones[1][1]) != 0 && mod4m(ones[2][0]
                                                                      - ones[2][1]) == 0)
                             || (mod4m(ones[0][0]
                                       - ones[0][1]) != 0
                                 && mod4m(ones[1][0]
                                          - ones[1][1]) == 0 && mod4m(ones[2][0]
                                                                      - ones[2][1]) == 0)) {

                        if (mod4m(ones[0][0]
                                  - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1])
                                + mod4m(ones[2][0]
                                        - ones[2][1]) == 2 * m - 4
                                || mod4m(ones[0][0]
                                         - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1])
                                + mod4m(ones[2][0]
                                        - ones[2][1]) == 2 * m - 2
                                || mod4m(ones[0][0]
                                         - ones[0][1])
                                + mod4m(ones[1][0]
                                        - ones[1][1])
                                + mod4m(ones[2][0]
                                        - ones[2][1]) == 2 * m) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                        // Creating the set K10'''~
                        if (mod4m(ones[0][1]
                                  - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0])
                                + mod4m(ones[2][1]
                                        - ones[2][0]) == 2 * m - 4
                                || mod4m(ones[0][1]
                                         - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0])
                                + mod4m(ones[2][1]
                                        - ones[2][0]) == 2 * m - 2
                                || mod4m(ones[0][1]
                                         - ones[0][0])
                                + mod4m(ones[1][1]
                                        - ones[1][0])
                                + mod4m(ones[2][1]
                                        - ones[2][0]) == 2 * m) {

                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }

                    }

                    // Creating the set K10''''
                    else {
                        if (m < 3) {
                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);


                        }
                    }

                    break;

                }

                default:
                    break;

                }
            } // end of non-sam-case

            // begin of samecase
            else {


                // begin of the 1. case
                if (j == -1 && l == -1 && t == -1) {
                    switch (mod2(i) + mod2(k) + mod2(s)) {
                        // S_1
                    case 0: {



                        if ((i==0&&k==0&&s==0)||(i + k + s == 4 * m)
                                || (12 * m - (i + k + s) == 4 * m)) {


                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);
                        }
                    }

                    // S_3
                    case 2: {
                        if ((i + k + s == 4 * m)
                                || (12 * m - (i + k + s) == 4 * m)) {


                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);
                        }
                    }

                    // S_4
                    case 3: {
                        if (((i + k + s == 4 * m - 1) || (i + k + s == 4 * m + 1))
                                || ((12 * m - (i + k + s) == 4 * m - 1) || (12
                                        * m - (i + k + s) == 4 * m + 1))) {


                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);
                        }
                    }
                    default:
                        break;
                    }
                }

                // begin of the 2. case
                else if (t == -1 && j == k && (j != -1 && l != -1)) {
                    switch (mod2(s) + mod2(i) + mod2(l)) {
                        // T_1
                    case 0: {
                        if ((s + i - l == 0) || (4 * m - (s + i - l) == 0)) {


                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);
                        }
                        if (l == 0) {
                            if (s + i  == 4*m) {

                                Configuration tmp(i,j,k,l,s,t);
                                compositionTable.insert(tmp);
                            }

                        }


                    }

                    // T_3
                    case 2: {
                        if ((s + i - l == 0) || (4 * m - (s + i - l) == 0)) {


                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);
                        }

                        if (l == 0) {
                            if (s + i  == 4*m) {

                                Configuration tmp(i,j,k,l,s,t);
                                compositionTable.insert(tmp);
                            }

                        }
                    }

                    // T_4
                    case 3: {
                        if (((s + i - l == -1) || (s + i - l == 1))
                                || ((4 * m - (s + i - l) == -1) || (4 * m
                                                                     - (s + i - l) == 1))) {


                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);
                        }
                    }
                    default:
                        break;
                    }
                } else if (j == -1 && l == s && (l != -1 && t != -1)) {
                    switch (mod2(i) + mod2(k) + mod2(t)) {
                        // T_1
                    case 0: {
                        if ((i + k - t == 0) || (4 * m - (i + k - t) == 0)) {


                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);
                        }

                        if (t == 0) {
                            if (i + k  == 4*m) {

                                Configuration tmp(i,j,k,l,s,t);
                                compositionTable.insert(tmp);
                            }

                        }
                    }

                    // T_3
                    case 2: {
                        if ((i + k - t == 0) || (4 * m - (i + k - t) == 0)) {


                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);
                        }
                        if (t == 0) {
                            if (i + k  == 4*m) {

                                Configuration tmp(i,j,k,l,s,t);
                                compositionTable.insert(tmp);
                            }

                        }

                    }

                    // T_4
                    case 3: {
                        if (((i + k - t == -1) || (i + k - t == 1))
                                || ((4 * m - (i + k - t) == -1) || (4 * m
                                                                     - (i + k - t) == 1))) {


                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);
                        }
                    }
                    default:
                        break;
                    }

                } else if (l == -1 && t == i && (t != -1 && j != -1)) {




                    switch (mod2(k) + mod2(s) + mod2(j)) {
                        // T_1
                    case 0: {
                        if ((k + s - j == 0) || (4 * m - (k + s - j) == 0)) {


                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);
                        }

                        if (j == 0) {
                            if (k + s  == 4*m) {

                                Configuration tmp(i,j,k,l,s,t);
                                compositionTable.insert(tmp);
                            }

                        }
                    }

                    // T_3
                    case 2: {
                        if ((k + s - j == 0) || (4 * m - (k + s - j) == 0)) {


                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);
                        }
                        if (j == 0) {
                            if (k + s  == 4*m) {

                                Configuration tmp(i,j,k,l,s,t);
                                compositionTable.insert(tmp);
                            }

                        }
                    }

                    // T_4
                    case 3: {
                        if (((k + s - j == -1) || (k + s - j == 1))
                                || ((4 * m - (k + s - j) == -1) || (4 * m
                                                                     - (k + s - j) == 1))) {


                            Configuration tmp(i,j,k,l,s,t);
                            compositionTable.insert(tmp);
                        }
                    }
                    default:
                        break;
                    }

                }
            }

        }



    return compositionTable;


}


// returns the converse of the given relation.
// note that the granularity m doesn't have any
// influence on the converse relation.
std::string getConverse(int mm, int i, int j) {

    string cmpString;
    
	m = mm;
	
    if (j!=-1) {
        cmpString = int2str(j) + "_" + int2str(i);
    }

    // converse of same-relation
    else {
        cmpString = "s_" + int2str(mod4m(4*m-i));
    }
    return cmpString;

}

// returns compositions of the table in string.
std::string OpraOnTheFly::getComposition(std::set<Configuration,Misc::conflt> compositionTable) {

    string cmpString = "";

    set<Configuration,Misc::conflt>::iterator pos = compositionTable.begin();

    // This temporary iterator prevents multiple outputs of same configurations
    set<Configuration,Misc::conflt>::iterator tmp1 = compositionTable.begin();





    /*
        cmpString += "( ";
        if (compositionTable.size() != 0) {
            if((*pos).t!=-1) {
                cmpString +=  int2str((*pos).t) + "_" + int2str((*pos).s);
                cmpString += " ";
            } else {
                cmpString += "s_" + int2str(mod4m(4*m-(*pos).s));
                cmpString +=  " ";
            }
            *pos++;
        }
     
     
        while (pos != compositionTable.end()) {
     
            if (((*tmp1).i != (*pos).i)|| ((*tmp1).j != (*pos).j)|| ((*tmp1).k != (*pos).k)|| ((*tmp1).l != (*pos).l)|| ((*tmp1).s != (*pos).s)|| ((*tmp1).t != (*pos).t))  {
                if((*pos).t!=-1) {
                    cmpString +=  int2str((*pos).t) + "_" + int2str((*pos).s);
                    cmpString += " ";
                } else {
                    cmpString += "s_" + int2str(mod4m(4*m-(*pos).s));
                    cmpString +=  " ";
                }
     
                tmp1 = pos;
            }
            pos ++;
        }
        cmpString +=")";
    */


    cmpString += "( ";


    while (pos != compositionTable.end()) {


        if((*pos).t!=-1) {
            cmpString +=  int2str((*pos).t) + "_" + int2str((*pos).s);
            cmpString += " ";
        } else {
            cmpString += "s_" + int2str(mod4m(4*m-(*pos).s));
            cmpString +=  " ";
        }



        pos ++;
    }
    cmpString +=")";




    return cmpString;

}



// prints the compositions in the table
void OpraOnTheFly::printComposition(std::set<Configuration,Misc::conflt> compositionTable) {

    cout << OpraOnTheFly::getComposition(compositionTable) <<"\n\n";

    cout << "Number of elements: " << compositionTable.size() << endl;
}




std::string int2str(int nr) {
    if (nr==-1)
        return "s";
    else {
        char res_s[10];
        int len;
        len = sprintf(res_s,"%i",nr);
        std::string res = res_s;
        return res;
    }
}
