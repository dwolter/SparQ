/* This file is part of SparQ, a toolbox for qualitative spatial reasoning.
   Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
   More info at http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

  SparQ is free software and has been released under the terms of the GNU
  General Public License version 3 or later. You should have received a
  copy of the GNU General Public License along with this program. If not,
  see <http://www.gnu.org/licenses/>.
*/

#include "qualifier.h"
#include <vector>
#include <string>
#include <point.h>
#include <cangle.h>
#include <vector.h>
#include <line.h>
#include <stdio.h>
#include <stdlib.h>
using namespace std;

int main(int argc, char *argv[]) {

    //     bool testGPCC=true;
    bool testGPCC=false;
    //bool testGPCC_CNH=true;
         bool testGPCC_CNH=false;
    //     bool testGTPCC=true;
    bool testGTPCC=false;
    //bool testGTPCC_CNH=true;
         bool testGTPCC_CNH=false;
         bool testOPRA=false;
    //bool testOPRA=true;
    //     bool testDipole=false;
    bool testDipole=false;
         bool testSCC=true;
    //bool testSCC=false;
    //     bool testDCC=true;
    bool testDCC=false;
    //     bool testTPCC=true;
    bool testTPCC=false;
    // bool testPBADC=true;
    bool testPBADC=false;
    //bool testQTC=true;
    bool testQTC=false;

    std::vector< int > result;
    int m=4;
    double Ax=0.0;
    double Ay=0.0;
    double Aori=0.0;
    double Bx=1.0;
    double By=0.0;
    double Bori=0.0;    // in rad
    double Cx=1.0;
    double Cy=0.0;
    double Cori=0.0;    // in rad

    if( testGPCC ) {
        cout << "*** GPCC ***" << endl;
        m=1;
        Ax = 0.0;
        Ay = 0.0;
        Bx = 1.0;
        By = 0.0;
        Cx = 1.0;
        Cy = 1.0;
        cout << "(" << Ax << "," << Ay << ")(" << Bx << "," << By << ")(" << Cx << "," << Cy << ")_" << m << " =(2_3)> ";
        cout << getGPCC_Relation_String( m, Ax, Ay, Bx, By, Cx, Cy) << endl;
        m=2;
        Ax = 0.0;
        Ay = 0.0;
        Bx = 1.0;
        By = 0.0;
        Cx = 1.0;
        Cy = 1.0;
        cout << "(" << Ax << "," << Ay << ")(" << Bx << "," << By << ")(" << Cx << "," << Cy << ")_" << m << " =(3_5)> ";
        cout << getGPCC_Relation_String( m, Ax, Ay, Bx, By, Cx, Cy) << endl;

    }
    if( testGPCC_CNH ) {
        cout << "*** GPCC_CNH ***" << endl;
        // [0..4m-1,1..8m]
        std::vector< int > relation;
        std::string sRelation;
        char buffer[50];
        int m = 2;
        cout << "List of all GPCC base relations {tri, dou, sam, [1..4m-1,0..8m] : " << getGPCC_All_Base_Relations_String( m ) << endl;
        relation.resize(2);
        relation[0] = 0;
        relation[1] = -1;
        cout << "[" << relation[0] << "_" << relation[1] << "]_" << m << " : " << getGPCC_CNH_Relations_String( m, relation ) << endl;
        sRelation = "tri";
        cout << sRelation << " : " << getGPCC_CNH_Relations_String( m, sRelation ) << endl;
        relation[0] = 0;
        relation[1] = -2;
        cout << "[" << relation[0] << "_" << relation[1] << "]_" << m << " : " << getGPCC_CNH_Relations_String( m, relation ) << endl;
        sRelation = "dou";
        cout << sRelation << " : " << getGPCC_CNH_Relations_String( m, sRelation ) << endl;
        relation[0] = 0;
        relation[1] = -3;
        cout << "[" << relation[0] << "_" << relation[1] << "]_" << m << " : " << getGPCC_CNH_Relations_String( m, relation ) << endl;
        sRelation = "sam";
        cout << sRelation << " : " << getGPCC_CNH_Relations_String( m, sRelation ) << endl;
        relation[0] = 2;
        relation[1] = 2;
        cout << "[" << relation[0] << "_" << relation[1] << "]_" << m << " : " << getGPCC_CNH_Relations_String( m, relation ) << endl;
        sprintf( buffer, "%d_%d\0", relation[0], relation[1]);
        sRelation = buffer;
        cout << sRelation << " : " << getGPCC_CNH_Relations_String( m, sRelation ) << endl;
        relation[0] = 4*m;
        relation[1] = 2;
        cout << "[" << relation[0] << "_" << relation[1] << "]_" << m << " : " << getGPCC_CNH_Relations_String( m, relation ) << endl;
        sprintf( buffer, "%d_%d\0", relation[0], relation[1]);
        sRelation = buffer;
        cout << sRelation << " : " << getGPCC_CNH_Relations_String( m, sRelation ) << endl;
        relation[0] = 2;
        relation[1] = 8*m;
        cout << "[" << relation[0] << "_" << relation[1] << "]_" << m << " : " << getGPCC_CNH_Relations_String( m, relation ) << endl;
        sprintf( buffer, "%d_%d\0", relation[0], relation[1]);
        sRelation = buffer;
        cout << sRelation << " : " << getGPCC_CNH_Relations_String( m, sRelation ) << endl;
        relation[0] = 4*m-1;
        relation[1] = 1;
        cout << "[" << relation[0] << "_" << relation[1] << "]_" << m << " : " << getGPCC_CNH_Relations_String( m, relation ) << endl;
        sprintf( buffer, "%d_%d\0", relation[0], relation[1]);
        sRelation = buffer;
        cout << sRelation << " : " << getGPCC_CNH_Relations_String( m, sRelation ) << endl;
        relation[0] = 4*m-1;
        relation[1] = 8*m-1;
        cout << "[" << relation[0] << "_" << relation[1] << "]_" << m << " : " << getGPCC_CNH_Relations_String( m, relation ) << endl;
        sprintf( buffer, "%d_%d\0", relation[0], relation[1]);
        sRelation = buffer;
        cout << sRelation << " : " << getGPCC_CNH_Relations_String( m, sRelation ) << endl;
    }
    if( testGTPCC ) {
        cout << "*** GTPCC ***" << endl;
        m=1;
        Ax = 0.0;
        Ay = 0.0;
        Bx = 1.0;
        By = 0.0;
        Cx = 1.0;
        Cy = 1.0;
        cout << "(" << Ax << "," << Ay << ")(" << Bx << "," << By << ")(" << Cx << "," << Cy << ")_" << m
        << " =(1_1 1_2 2_1 2_2)> ";
        cout << getGTPCC_Relation_String( m, Ax, Ay, Bx, By, Cx, Cy) << endl;
        m=2;
        Ax = 0.0;
        Ay = 0.0;
        Bx = 1.0;
        By = 0.0;
        Cx = 1.0;
        Cy = 1.0;
        cout << "(" << Ax << "," << Ay << ")(" << Bx << "," << By << ")(" << Cx << "," << Cy << ")_" << m
        << "=(2_2 2_3 3_2 3_3)> ";
        cout << getGTPCC_Relation_String( m, Ax, Ay, Bx, By, Cx, Cy) << endl;
    }
    if( testGTPCC_CNH ) {
        cout << "*** GTPCC_CNH ***" << endl;
        // [0..2m,1..4m]
        std::vector< int > relation;
        std::string sRelation;
        char buffer[50];
        int m = 2;
        cout << "List of all GTPCC base relations {tri, dou, sam, [1..2m,0..4m-1] : " << getGTPCC_All_Base_Relations_String( m ) << endl;
        ;
        relation.resize(2);
        relation[0] = 0;
        relation[1] = -1;
        cout << "[" << relation[0] << "_" << relation[1] << "]_" << m << " : " << getGTPCC_CNH_Relations_String( m, relation ) << endl;
        sRelation = "tri";
        cout << sRelation << " : " << getGTPCC_CNH_Relations_String( m, sRelation ) << endl;
        relation[0] = 0;
        relation[1] = -2;
        cout << "[" << relation[0] << "_" << relation[1] << "]_" << m << " : " << getGTPCC_CNH_Relations_String( m, relation ) << endl;
        sRelation = "dou";
        cout << sRelation << " : " << getGTPCC_CNH_Relations_String( m, sRelation ) << endl;
        relation[0] = 0;
        relation[1] = -3;
        cout << "[" << relation[0] << "_" << relation[1] << "]_" << m << " : " << getGTPCC_CNH_Relations_String( m, relation ) << endl;
        sRelation = "sam";
        cout << sRelation << " : " << getGTPCC_CNH_Relations_String( m, sRelation ) << endl;
        relation[0] = 2;
        relation[1] = 2;
        cout << "[" << relation[0] << "_" << relation[1] << "]_" << m << " : " << getGTPCC_CNH_Relations_String( m, relation ) << endl;
        sprintf( buffer, "%d_%d\0", relation[0], relation[1]);
        sRelation = buffer;
        cout << sRelation << " : " << getGTPCC_CNH_Relations_String( m, sRelation ) << endl;
        relation[0] = 2*m+1;
        relation[1] = 2;
        cout << "[" << relation[0] << "_" << relation[1] << "]_" << m << " : " << getGTPCC_CNH_Relations_String( m, relation ) << endl;
        sprintf( buffer, "%d_%d\0", relation[0], relation[1]);
        sRelation = buffer;
        cout << sRelation << " : " << getGTPCC_CNH_Relations_String( m, sRelation ) << endl;
        relation[0] = 2;
        relation[1] = 4*m;
        cout << "[" << relation[0] << "_" << relation[1] << "]_" << m << " : " << getGTPCC_CNH_Relations_String( m, relation ) << endl;
        sprintf( buffer, "%d_%d\0", relation[0], relation[1]);
        sRelation = buffer;
        cout << sRelation << " : " << getGTPCC_CNH_Relations_String( m, sRelation ) << endl;
        relation[0] = 2*m;
        relation[1] = 1;
        cout << "[" << relation[0] << "_" << relation[1] << "]_" << m << " : " << getGTPCC_CNH_Relations_String( m, relation ) << endl;
        sprintf( buffer, "%d_%d\0", relation[0], relation[1]);
        sRelation = buffer;
        cout << sRelation << " : " << getGTPCC_CNH_Relations_String( m, sRelation ) << endl;
        relation[0] = 2*m;
        relation[1] = 4*m-1;
        cout << "[" << relation[0] << "_" << relation[1] << "]_" << m << " : " << getGTPCC_CNH_Relations_String( m, relation ) << endl;
        sprintf( buffer, "%d_%d\0", relation[0], relation[1]);
        sRelation = buffer;
        cout << sRelation << " : " << getGTPCC_CNH_Relations_String( m, sRelation ) << endl;
    }



    int max= getOPRA_MaxSectors( m );

    Point ptAStart( 1,1 );
    Point ptAEnd( 2,2 );

    Point ptBStart( 2,2 );
    Point ptBEnd( 1,1 );

    Point ptRef( 10, 0 );
    Point ptRef1( 4, 4 );
    Point ptRef2( 1.5, 1.5 );
    Point ptRef3( -1, -1 );
    //     Line lineA( ptAStart, ptAEnd);
    //     Line lineB( ptBStart, ptBEnd);
    //
    //     cout << "Seite bzgl. A: " << lineA.GetSide( ptRef ) << endl;
    //     cout << "Seite bzgl. B: " << lineB.GetSide( ptRef ) << endl;
    //
    //     cout << lineA << " : " << lineA.PositionOnLineSegment( ptRef ) << endl;
    //     cout << lineA << " : " << lineA.PositionOnLineSegment( ptRef1 ) << endl;
    //     cout << lineA << " : " << lineA.PositionOnLineSegment( ptRef2 ) << endl;
    //     cout << lineA << " : " << lineA.PositionOnLineSegment( ptRef3 ) << endl;
    //     cout << endl;
    //     cout << lineA << " : " << lineA.PositionOnLineSegment( ptRef ) << endl;
    //     cout << lineB << " : " << lineB.PositionOnLineSegment( ptRef1 ) << endl;
    //     cout << lineB << " : " << lineB.PositionOnLineSegment( ptRef2 ) << endl;
    //     cout << lineB << " : " << lineB.PositionOnLineSegment( ptRef3 ) << endl;

    if( testDipole ) {
        cout << "*** Dipole Calculus ***" << endl;
        m = DRA_fp;
        std::vector< char > resultDC;
        cout << "31: seseP?  -> ";
        resultDC = getDC_Relation( m, Point(-1,-1), Point(-1, 1), Point(-1,-1), Point(-1, 1));
        for( int i=0; i<resultDC.size(); i++) {
            cout << resultDC[i];
        }
        cout << "   -(string)-> " << getDC_Relation_String( m, Point(-1,-1), Point(-1, 1), Point(-1,-1), Point(-1, 1));
        cout << endl;
        cout << "32: esesA?  -> ";
        resultDC = getDC_Relation( m, Point(-1,-1), Point(-1, 1), Point(-1, 1), Point(-1,-1));
        for( int i=0; i<resultDC.size(); i++) {
            cout << resultDC[i];
        }
        cout << "   -(string)-> " << getDC_Relation_String( m, Point(-1,-1), Point(-1, 1), Point(-1, 1), Point(-1,-1));
        ;
        cout << endl;
        cout << "11: llllA?  -> ";
        resultDC = getDC_Relation( m, Point(1,-1), Point(1, 1), Point(-1, 1), Point(-1,-1));
        for( int i=0; i<resultDC.size(); i++) {
            cout << resultDC[i];
        }
        cout << "   -(string)-> " << getDC_Relation_String( m, Point(1,-1), Point(1, 1), Point(-1, 1), Point(-1,-1));
        cout << endl;
        cout << "12: llll-?  -> ";
        resultDC = getDC_Relation( m, Point(1,-1), Point(1, 1), Point(-2, 1), Point(-1,-1));
        for( int i=0; i<resultDC.size(); i++) {
            cout << resultDC[i];
        }
        cout << "   -(string)-> " << getDC_Relation_String( m, Point(1,-1), Point(1, 1), Point(-2, 1), Point(-1,-1));
        cout << endl;
        cout << "10: llll+?  -> ";
        resultDC = getDC_Relation( m, Point(1,-1), Point(1, 1), Point(-1, 1), Point(-2,-1));
        for( int i=0; i<resultDC.size(); i++) {
            cout << resultDC[i];
        }
        cout << "   -(string)-> " << getDC_Relation_String( m, Point(1,-1), Point(1, 1), Point(-1, 1), Point(-2,-1));
        cout << endl;
        cout << "15: rlrr+?  -> ";
        resultDC = getDC_Relation( m, Point(0,1), Point(0, 2), Point(1, -1), Point(-1,-1));
        for( int i=0; i<resultDC.size(); i++) {
            cout << resultDC[i];
        }
        cout << "   -(string)-> " << getDC_Relation_String( m, Point(0,1), Point(0, 2), Point(1, -1), Point(-1,-1));
        cout << endl;
        cout << "13: rrrl-?  -> ";
        resultDC = getDC_Relation( m, Point(0,0), Point(0, 2), Point(1, 1), Point(2,1));
        for( int i=0; i<resultDC.size(); i++) {
            cout << resultDC[i];
        }
        cout << "   -(string)-> " << getDC_Relation_String( m, Point(0,0), Point(0, 2), Point(1, 1), Point(2,1));
        cout << endl;
        cout << "23: ells+?  -> ";
        resultDC = getDC_Relation( m, Point(1,1), Point(1, 2), Point(1, 2), Point(-1,3));
        for( int i=0; i<resultDC.size(); i++) {
            cout << resultDC[i];
        }
        cout << "   -(string)-> " << getDC_Relation_String( m, Point(1,1), Point(1, 2), Point(1, 2), Point(-1,3));
        cout << endl;
        cout << "28: srsl-?  -> ";
        resultDC = getDC_Relation( m, Point(-1,-1), Point(-1, 2), Point(-1, -1), Point(1,-1));
        for( int i=0; i<resultDC.size(); i++) {
            cout << resultDC[i];
        }
        cout << "   -(string)-> " << getDC_Relation_String( m, Point(-1,-1), Point(-1, 2), Point(-1, -1), Point(1,-1));
        cout << endl;
        cout << "28: srsl-?  -> ";
        resultDC = getDC_Relation( m, Point(1,1), Point(1, -2), Point(1, 1), Point(-1,-1));
        for( int i=0; i<resultDC.size(); i++) {
            cout << resultDC[i];
        }
        cout << "   -(string)-> " << getDC_Relation_String( m, Point(1,1), Point(1, -2), Point(1, 1), Point(-1,-1));
        cout << endl;
        cout << "41: biifP?  -> ";
        resultDC = getDC_Relation( m, Point(-1,1), Point(-1, 3), Point(-1, -1), Point(-1,2));
        for( int i=0; i<resultDC.size(); i++) {
            cout << resultDC[i];
        }
        cout << "   -(string)-> " << getDC_Relation_String( m, Point(-1,1), Point(-1, 3), Point(-1, -1), Point(-1,2));
        cout << endl;
        cout << "41: biifP?  -> ";
        resultDC = getDC_Relation( m, Point(1,1), Point(3, 3), Point(-1, -1), Point(2,2));
        for( int i=0; i<resultDC.size(); i++) {
            cout << resultDC[i];
        }
        cout << "   -(string)-> " << getDC_Relation_String( m, Point(1,1), Point(3, 3), Point(-1, -1), Point(2,2));
        cout << endl;
        cout << "41: biifP?  -> ";
        resultDC = getDC_Relation( m, Point(1,1), Point(-3, -3), Point(2, 2), Point(-2,-2));
        for( int i=0; i<resultDC.size(); i++) {
            cout << resultDC[i];
        }
        cout << "   -(string)-> " << getDC_Relation_String( m, Point(1,1), Point(-3, -3), Point(2, 2), Point(-2,-2));
        cout << endl;
        cout << "62: lfrr-?  -> ";
        resultDC = getDC_Relation( m, Point(1,-1), Point(1, 1), Point(-1,3), Point(1, 2));
        for( int i=0; i<resultDC.size(); i++) {
            cout << resultDC[i];
        }
        cout << "   -(string)-> " << getDC_Relation_String( m, Point(1,-1), Point(1, 1), Point(-1,3), Point(1, 2));
        cout << endl;
    }
    /*****************************************************************************
    *** OPRA polar functions test
    *****************************************************************************/
    if( testOPRA ) {
        cout << endl << "*** OPRA ***" << endl;
        m = 2;
        cout << " m = " << m << endl;
        std::string sResult;

        Ax=1.0;
        Ay=1.0;
        Aori=0.0;
        Bx=1.0;
        By=1.0;
        Bori=0.0;
        result = getOPRA_Relation( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << "Result [0] (" << result.size() << "): " << result[0] << " : " << result[1] << endl;
        sResult = getOPRA_Relation_String( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << sResult << endl;

        Ax=1.0;
        Ay=2.0;
        Aori=0.0;
        Bx=1.0;
        By=2.0;
        Bori=M_PI;
        result = getOPRA_Relation( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << "Result 2[4] (" << result.size() << "): " << result[0] << " : " << result[1] << endl;
        sResult = getOPRA_Relation_String( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << sResult << endl;

        Ax=1.0;
        Ay=2.0;
        Aori=0.0;
        Bx=1.0;
        By=2.0;
        Bori=M_PI/2;
        result = getOPRA_Relation( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << "Result 2[2] (" << result.size() << "): " << result[0] << " : " << result[1] << endl;
        sResult = getOPRA_Relation_String( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << sResult << endl;

        Ax=1.0;
        Ay=2.0;
        Aori=0.0;
        Bx=1.0;
        By=2.0;
        Bori=-M_PI/2;
        result = getOPRA_Relation( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << "Result 2[6] (" << result.size() << "): " << result[0] << " : " << result[1] << endl;
        sResult = getOPRA_Relation_String( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << sResult << endl;

        Ax=1.0;
        Ay=2.0;
        Aori=0.0;
        Bx=1.0;
        By=2.0;
        Bori=3*M_PI/4;
        result = getOPRA_Relation( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << "Result 2[3] (" << result.size() << "): " << result[0] << " : " << result[1] << endl;
        sResult = getOPRA_Relation_String( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << sResult << endl;

        Ax=1.0;
        Ay=1.0;
        Aori=0.0;
        Bx=2.0;
        By=2.0;
        Bori=0.0;
        result = getOPRA_Relation( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << "Result 2[1,5] (" << result.size() << "): " << result[0] << " : " << result[1] << endl;
        sResult = getOPRA_Relation_String( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << sResult << endl;

        Ax=1.0;
        Ay=1.0;
        Aori=0.0;
        Bx=2.0;
        By=2.0;
        Bori=-M_PI;
        result = getOPRA_Relation( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << "Result 2[1,1] (" << result.size() << "): " << result[0] << " : " << result[1] << endl;
        sResult = getOPRA_Relation_String( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << sResult << endl;

        Ax=1.0;
        Ay=1.0;
        Aori=0.0;
        Bx=2.0;
        By=2.0;
        Bori=M_PI;
        result = getOPRA_Relation( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << "Result 2[1,1] (" << result.size() << "): " << result[0] << " : " << result[1] << endl;
        sResult = getOPRA_Relation_String( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << sResult << endl;

        Ax=1.0;
        Ay=1.0;
        Aori=0.0;
        Bx=2.0;
        By=2.0;
        Bori=M_PI/2;
        result = getOPRA_Relation( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << "Result 2[1,3] (" << result.size() << "): " << result[0] << " : " << result[1] << endl;
        sResult = getOPRA_Relation_String( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << sResult << endl;

        Ax=1.0;
        Ay=1.0;
        Aori=0.0;
        Bx=2.0;
        By=2.0;
        Bori=-M_PI/2;
        result = getOPRA_Relation( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << "Result 2[1,7] (" << result.size() << "): " << result[0] << " : " << result[1] << endl;
        sResult = getOPRA_Relation_String( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << sResult << endl;

        m=3;
        Point ptRef( 1.0, 0.0);
        ptRef.Rotate( M_PI/3.0 );
        // cout << "ptRef: " << ptRef << endl;
        Ax=0.0;
        Ay=0.0;
        Aori=0.0;
        Bx=ptRef.X();
        By=ptRef.Y();
        Bori=-M_PI*2.0/3.0;
        result = getOPRA_Relation( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << "Result 3[2,0] (" << result.size() << "): " << result[0] << " : " << result[1] << endl;
        sResult = getOPRA_Relation_String( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << sResult << endl;

        ptRef.Rotate( -M_PI*2.0/3.0 );
        Ax=0.0;
        Ay=0.0;
        Aori=0.0;
        Bx=ptRef.X();
        By=ptRef.Y();
        Bori=M_PI/3.0;
        result = getOPRA_Relation( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << "Result 3[10,2] (" << result.size() << "): " << result[0] << " : " << result[1] << endl;
        sResult = getOPRA_Relation_String( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << sResult << endl;

        m=4;
        Ax=0.0;
        Ay=0.0;
        Aori=0.0;
        Bx=1.0;
        By=0.0;
        Bori=M_PI;
        result = getOPRA_Relation( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << "Result 4[0,0] (" << result.size() << "): " << result[0] << " : " << result[1] << endl;
        sResult = getOPRA_Relation_String( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << sResult << endl;

        Ax=0.0;
        Ay=0.0;
        Aori=0.0;
        Bx=1.0;
        By=1.0;
        Bori=-M_PI*3.0/4.0;
        result = getOPRA_Relation( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << "Result 4[2,0] (" << result.size() << "): " << result[0] << " : " << result[1] << endl;
        sResult = getOPRA_Relation_String( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << sResult << endl;

        Ax=0.0;
        Ay=0.0;
        Aori=0.0;
        Bx=0.0;
        By=-1.0;
        Bori=M_PI/2.0;
        result = getOPRA_Relation( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << "Result 4[12,0] (" << result.size() << "): " << result[0] << " : " << result[1] << endl;
        sResult = getOPRA_Relation_String( m, Ax, Ay, Aori, Bx, By, Bori);
        cout << sResult << endl;


        /*****************************************************************************
        *** OPRA cartesian functions test
        *****************************************************************************/
        //   Ax = 1.0;
        //   Ay = 1.0;
        //   double Adx = 1.0;
        //   double Ady = 1.0;
        //   Bx = 2.0;
        //   By = 2.0;
        //   double Bdx = 1.0;
        //   double Bdy = 1.0;
        //
        //   result = getOPRA_Relation(m, Ax, Ay, Adx, Ady, Bx, By, Bdx, Bdy);
        //   cout << "Result: " << result[0] << " : " << result[1] << endl;
    }

    if( testSCC ) {
        cout << "*** SCC ***" << endl;
        int m=1;
        Ax = -1.0;
        Ay = 1.0;
        Bx = 1.0;
        By = 1.0;
        Cx = 2.0;
        Cy = 1.0;
        cout << "SCC_2[0]: " << getSCC_Relation_String( m, Ax, Ay, Bx, By, Cx, Cy) << endl;
        Ax = -1.0;
        Ay = 1.0;
        Bx = 1.0;
        By = 1.0;
        Cx = -2.0;
        Cy = 2.0;
        cout << "SCC_2[3]: " << getSCC_Relation_String( m, Ax, Ay, Bx, By, Cx, Cy) << endl;
        Ax = -1.0;
        Ay = 1.0;
        Bx = 1.0;
        By = 1.0;
        Cx = 2.0;
        Cy = 2.0;
        cout << "SCC_2[1]: " << getSCC_Relation_String( m, Ax, Ay, Bx, By, Cx, Cy) << endl;
        Ax = -1.0;
        Ay = 1.0;
        Bx = 1.0;
        By = 1.0;
        Cx = -1.0;
        Cy = -1.0;
        cout << "SCC_2[5]: " << getSCC_Relation_String( m, Ax, Ay, Bx, By, Cx, Cy) << endl;
    }
    if( testDCC ) {
        cout << "*** DCC ***" << endl;
        int m=2;
        Ax = 1.0;
        Ay = 1.0;
        Bx = 1.0;
        By = 1.0;
        Cx = 3.0;
        Cy = 3.0;
        cout << "DCC_2[i]: " << getDCC_Relation_String( m, Ax, Ay, Bx, By, Cx, Cy) << endl;
        Ax = 1.0;
        Ay = 1.0;
        Bx = 2.0;
        By = 1.0;
        Cx = 1.0;
        Cy = 1.0;
        cout << "DCC_2[4_a]: " << getDCC_Relation_String( m, Ax, Ay, Bx, By, Cx, Cy) << endl;
        Ax = 1.0;
        Ay = 1.0;
        Bx = 2.0;
        By = 1.0;
        Cx = 2.0;
        Cy = 1.0;
        cout << "DCC_2[b_4]: " << getDCC_Relation_String( m, Ax, Ay, Bx, By, Cx, Cy) << endl;
        Ax = 1.0;
        Ay = 1.0;
        Bx = 2.0;
        By = 1.0;
        Cx = 1.5;
        Cy = 1.0;
        cout << "DCC_2[4_4]: " << getDCC_Relation_String( m, Ax, Ay, Bx, By, Cx, Cy) << endl;
        Ax = 1.0;
        Ay = 1.0;
        Bx = 2.0;
        By = 1.0;
        Cx = 1.5;
        Cy = 1.5;
        cout << "DCC_2[3_5]: " << getDCC_Relation_String( m, Ax, Ay, Bx, By, Cx, Cy) << endl;
        Ax = 1.0;
        Ay = 1.0;
        Bx = 2.0;
        By = 1.0;
        Cx = 3.0;
        Cy = 2.0;
        cout << "DCC_2[1_5]: " << getDCC_Relation_String( m, Ax, Ay, Bx, By, Cx, Cy) << endl;
        Ax = 1.0;
        Ay = 1.0;
        Bx = 2.0;
        By = 1.0;
        Cx = -1.0;
        Cy = 1.0;
        cout << "DCC_2[4_0]: " << getDCC_Relation_String( m, Ax, Ay, Bx, By, Cx, Cy) << endl;
    }

    if( testTPCC ) {
        cout << "*** TPCC ***" << endl;
        cout << "dou : " << getTPCC_Relation_String( -1.0, 1.0, -1.0, 1.0, 1.0, 0.0) << endl;
        cout << "tri : " << getTPCC_Relation_String( -1.0, 0.0, -1.0, 0.0, -1.0, 0.0) << endl;
        cout << "sam : " << getTPCC_Relation_String( -1.0, 1.0, 1.0, 0.0, 1.0, 0.0) << endl;
        cout << endl;
        cout << "csb : " << getTPCC_Relation_String( -1.0, 1.0, 1.0, 1.0, 2.0, 1.0) << endl;
        cout << "clb : " << getTPCC_Relation_String( -1.0, 1.0, 1.0, 1.0, 2.0, 1.5) << endl;
        cout << "    : " << getTPCC_Relation_String( -1.0, 1.0, 1.0, 1.0, 2.0, 2.0) << endl;
        cout << "cbl : " << getTPCC_Relation_String( -1.0, 1.0, 1.0, 1.0, 1.5, 2.0) << endl;
        cout << "csl : " << getTPCC_Relation_String( -1.0, 1.0, 1.0, 1.0, 1.0, 2.0) << endl;
        cout << "cfl : " << getTPCC_Relation_String( -1.0, 1.0, 1.0, 1.0, 0.5, 2.0) << endl;
        cout << "    : " << getTPCC_Relation_String( -1.0, 1.0, 1.0, 1.0, 0.0, 2.0) << endl;
        cout << "clf : " << getTPCC_Relation_String( -1.0, 1.0, 1.0, 1.0, 0.0, 1.5) << endl;
        cout << "csf : " << getTPCC_Relation_String( -1.0, 1.0, 1.0, 1.0, 0.0, 1.0) << endl;
        cout << "crf : " << getTPCC_Relation_String( -1.0, 1.0, 1.0, 1.0, 0.0, 0.5) << endl;
        cout << "    : " << getTPCC_Relation_String( -1.0, 1.0, 1.0, 1.0, 0.0, 0.0) << endl;
        cout << "cfr : " << getTPCC_Relation_String( -1.0, 1.0, 1.0, 1.0, 0.5, 0.0) << endl;
        cout << "csr : " << getTPCC_Relation_String( -1.0, 1.0, 1.0, 1.0, 1.0, 0.0) << endl;
        cout << "cbr : " << getTPCC_Relation_String( -1.0, 1.0, 1.0, 1.0, 1.5, 0.0) << endl;
        cout << "    : " << getTPCC_Relation_String( -1.0, 1.0, 1.0, 1.0, 2.0, 0.0) << endl;
        cout << "crb : " << getTPCC_Relation_String( -1.0, 1.0, 1.0, 1.0, 2.0, 0.5) << endl;
        cout << endl;
        cout << "dsb : " << getTPCC_Relation_String( 0.5, 1.0, 1.0, 1.0, 2.0, 1.0) << endl;
        cout << "dlb : " << getTPCC_Relation_String( 0.5, 1.0, 1.0, 1.0, 2.0, 1.5) << endl;
        cout << "    : " << getTPCC_Relation_String( 0.5, 1.0, 1.0, 1.0, 2.0, 2.0) << endl;
        cout << "dbl : " << getTPCC_Relation_String( 0.5, 1.0, 1.0, 1.0, 1.5, 2.0) << endl;
        cout << "dsl : " << getTPCC_Relation_String( 0.5, 1.0, 1.0, 1.0, 1.0, 2.0) << endl;
        cout << "dfl : " << getTPCC_Relation_String( 0.5, 1.0, 1.0, 1.0, 0.5, 2.0) << endl;
        cout << "    : " << getTPCC_Relation_String( 0.5, 1.0, 1.0, 1.0, 0.0, 2.0) << endl;
        cout << "dlf : " << getTPCC_Relation_String( 0.5, 1.0, 1.0, 1.0, 0.0, 1.5) << endl;
        cout << "dsf : " << getTPCC_Relation_String( 0.5, 1.0, 1.0, 1.0, 0.0, 1.0) << endl;
        cout << "drf : " << getTPCC_Relation_String( 0.5, 1.0, 1.0, 1.0, 0.0, 0.5) << endl;
        cout << "    : " << getTPCC_Relation_String( 0.5, 1.0, 1.0, 1.0, 0.0, 0.0) << endl;
        cout << "dfr : " << getTPCC_Relation_String( 0.5, 1.0, 1.0, 1.0, 0.5, 0.0) << endl;
        cout << "dsr : " << getTPCC_Relation_String( 0.5, 1.0, 1.0, 1.0, 1.0, 0.0) << endl;
        cout << "dbr : " << getTPCC_Relation_String( 0.5, 1.0, 1.0, 1.0, 1.5, 0.0) << endl;
        cout << "    : " << getTPCC_Relation_String( 0.5, 1.0, 1.0, 1.0, 2.0, 0.0) << endl;
        cout << "drb : " << getTPCC_Relation_String( 0.5, 1.0, 1.0, 1.0, 2.0, 0.5) << endl;

    }

    if ( testPBADC ) {
        cout << endl << "*** PBADC ***" << endl;
        // std::string getPBADC_Relation_String( int m, double ptOriginX, double ptOriginY, double ptRelatumX, double ptRelatumY, float angleRelative=0);
        m=1;
        cout << " m=1; => 0 :  " << getPBADC_Relation_String( m, 1.0, 1.0, 2.0, 1.0) << endl;
        cout << " m=1; => 1 : " << getPBADC_Relation_String( m, 1.0, 1.0, 1.0, 2.0) << endl;
        cout << " m=1; => 2 : " << getPBADC_Relation_String( m, 1.0, 1.0, 0.0, 1.0) << endl;
        cout << " m=1; => 3 : " << getPBADC_Relation_String( m, 1.0, 1.0, 1.0, 0.0) << endl << endl;
        m=2;
        cout << " m=2; => 0 : " << getPBADC_Relation_String( m, 1.0, 1.0, 2.0, 1.0) << endl;
        cout << " m=2; => 1 : " << getPBADC_Relation_String( m, 1.0, 1.0, 2.0, 2.0) << endl;
        cout << " m=2; => 2 : " << getPBADC_Relation_String( m, 1.0, 1.0, 1.0, 2.0) << endl;
        cout << " m=2; => 3 : " << getPBADC_Relation_String( m, 1.0, 1.0, 0.0, 2.0) << endl;
        cout << " m=2; => 4 : " << getPBADC_Relation_String( m, 1.0, 1.0, 0.0, 1.0) << endl;
        cout << " m=2; => 5 : " << getPBADC_Relation_String( m, 1.0, 1.0, 0.0, 0.0) << endl;
        cout << " m=2; => 6 : " << getPBADC_Relation_String( m, 1.0, 1.0, 1.0, 0.0) << endl;
        cout << " m=2; => 7 : " << getPBADC_Relation_String( m, 1.0, 1.0, 2.0, 0.0) << endl << endl;
        m=3;
        cout << " m=3; => 0 : " << getPBADC_Relation_String( m, 0.0, 0.0, 3.0, 0.0) << endl;
        cout << " m=3; => 1 : " << getPBADC_Relation_String( m, 0.0, 0.0, 2.0, 1.5) << endl;
        cout << " m=3; => 2 : " << getPBADC_Relation_String( m, 0.0, 0.0, 2.0, 3.0) << endl;
        cout << " m=3; => 3 : " << getPBADC_Relation_String( m, 0.0, 0.0, 0.0, 3.0) << endl;
        cout << " m=3; => 4 : " << getPBADC_Relation_String( m, 0.0, 0.0, -2.0, 3.0) << endl;
        cout << " m=3; => 5 : " << getPBADC_Relation_String( m, 0.0, 0.0, -2.0, 1.5) << endl;
        cout << " m=3; => 6 : " << getPBADC_Relation_String( m, 0.0, 0.0, -3.0, 0.0) << endl;
        cout << " m=3; => 7 : " << getPBADC_Relation_String( m, 0.0, 0.0, -2.0, -1.5) << endl;
        cout << " m=3; => 8 : " << getPBADC_Relation_String( m, 0.0, 0.0, -2.0, -3.0) << endl;
        cout << " m=3; => 9 : " << getPBADC_Relation_String( m, 0.0, 0.0, 0.0, -3.0) << endl;
        cout << " m=3; => 10 : " << getPBADC_Relation_String( m, 0.0, 0.0, 2.0, -3.0) << endl;
        cout << " m=3; => 11 : " << getPBADC_Relation_String( m, 0.0, 0.0, 2.0, -1.5) << endl << endl;
        m=4;
        cout << " m=4; => 2 : " << getPBADC_Relation_String( m, 1.0, 1.0, 2.0, 2.0) << endl;
        cout << " m=4; => 4 : " << getPBADC_Relation_String( m, 1.0, 1.0, 1.0, 2.0) << endl << endl;
    }

   if( testQTC ){
      cout << "QTC base relations (81): " << getQTC_All_Base_Relations_String( ) << endl;
      double pt_K1_X = 1.0;
      double pt_K1_Y = 1.0;
      double pt_K2_X = 1.0;
      double pt_K2_Y = 1.0;
      double pt_L1_X = 1.0;
      double pt_L1_Y = 2.0;
      double pt_L2_X = 1.0;
      double pt_L2_Y = 2.0;
      cout << "K1: (" << pt_K1_X << "," << pt_K1_Y << ") K2: (" << pt_K2_X << "," << pt_K2_Y << ") "
           << "L1: (" << pt_L1_X << "," << pt_L1_Y << ") L2: (" << pt_L2_X << "," << pt_L2_Y << ") => ";
      cout << getQTC_Relation_String( pt_K1_X, pt_K1_Y, pt_K2_X, pt_K2_Y, 
                                      pt_L1_X, pt_L1_Y, pt_L2_X, pt_L2_Y) << endl;
      pt_K1_X = 1.0;
      pt_K1_Y = 1.0;
      pt_K2_X = 1.5;
      pt_K2_Y = 1.5;
      pt_L1_X = 1.5;
      pt_L1_Y = 2.5;
      pt_L2_X = 1.5;
      pt_L2_Y = 2.5;
      cout << "K1: (" << pt_K1_X << "," << pt_K1_Y << ") K2: (" << pt_K2_X << "," << pt_K2_Y << ") "
           << "L1: (" << pt_L1_X << "," << pt_L1_Y << ") L2: (" << pt_L2_X << "," << pt_L2_Y << ") => ";
      cout << getQTC_Relation_String( pt_K1_X, pt_K1_Y, pt_K2_X, pt_K2_Y, 
                                      pt_L1_X, pt_L1_Y, pt_L2_X, pt_L2_Y) << endl;
   }


    return EXIT_SUCCESS;
}
