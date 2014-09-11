/* This file is part of SparQ, a toolbox for qualitative spatial reasoning.
   Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
   More info at http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

  SparQ is free software and has been released under the terms of the GNU
  General Public License version 3 or later. You should have received a
  copy of the GNU General Public License along with this program. If not,
  see <http://www.gnu.org/licenses/>.
*/

#ifndef _QUALIFIER_CPP_
#define _QUALIFIER_CPP_


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <qualifier.h>
#include "gpcc.h"
#include "gtpcc.h"
#include <iostream>
#include <string>
#include <cstring>
#include <cstdlib>


using namespace std;
char cRes[1024];


/*****************************************************************************
*** OPRA
*****************************************************************************/
int getOPRA_MaxSectors( int m ) {
    return (4*m);
}


std::string getOPRA_Relation_String(int m,
                                    Point ptA, double oriA,
                                    Point ptB, double oriB) {
    std::vector< int > vResult = getOPRA_Relation(m, ptA, oriA, ptB, oriB);
    char cString[20];
    std::string sResult;
    if( vResult.size()==1 ) {
        sprintf( cString, "s_%d", vResult[0]);
        sResult = (std::string)cString;
    } else if( vResult.size()==2 ) {
        sprintf( cString, "%d_%d", vResult[0],vResult[1]);
        sResult = (std::string)cString;
    } else {
        sResult = ":error";
    }
    return sResult;

}


string getOPRA_Relation_String(int m,
                               const double Ax, const double Ay, double Aori,
                               const double Bx, const double By, double Bori) {
    std::vector< int > vResult = getOPRA_Relation(m, Ax, Ay, Aori, Bx, By, Bori);
    char cString[20];
    std::string sResult;
    if( vResult.size()==1 ) {
        sprintf( cString, "s_%d", vResult[0]);
        sResult = (std::string)cString;
    } else if( vResult.size()==2 ) {
        sprintf( cString, "%d_%d", vResult[0],vResult[1]);
        sResult = (std::string)cString;
    } else {
        sResult = ":error";
    }
    return sResult;

}


/*****************************************************
  This is the calculating function for OPRA segments 
  *****************************************************/
std::vector< int > getOPRA_Relation(int m,
                                    Point ptA, double oriA,
                                    Point ptB, double oriB) {
    int relAB, relBA;

    Point ptOrigin( 0,0 );

    std::vector< int > result;
    CAngle cangleA(oriA);
    CAngle cangleB(oriB);

    if( ptA == ptB ) {
        relBA = calculateOrientationSegmentOPRA( m, (cangleB-cangleA).get() );
        result.push_back( relBA );
    } else {
        Point ptB_1( ptB );
        ptB_1.Shift( ptOrigin-ptA );
        ptB_1.Rotate( -cangleA.get() );
        relAB = calculateOrientationSegmentOPRA( m, ptOrigin.GetAngleTo( ptB_1 ) );

        Point ptA_1( ptA );
        ptA_1.Shift( ptOrigin-ptB );
        ptA_1.Rotate( -cangleB.get() );
        relBA = calculateOrientationSegmentOPRA( m, ptOrigin.GetAngleTo( ptA_1 ) );

        result.push_back( relAB );
        result.push_back( relBA );
    }
    return result;
}


std::vector< int > getOPRA_Relation2(int m,
                                     Point ptA, double oriA,
                                     Point ptB, double oriB) {
    int relAB, relBA;
    std::vector< int > result;
    CAngle cangleA(oriA);
    CAngle cangleB(oriB);

    BB_DBG(2) << "Punkt A: " << ptA << ":" << cangleA << " & Punkt B: " << ptB << ":" << cangleB << endl;
    ptB.Rotate( -cangleA.get(), ptA );
    cangleB = cangleB - cangleA;
    BB_DBG(2) << "Punkt A: " << ptA << ":" << cangleA-cangleA << " & Punkt B: " << ptB << ":" << cangleB << endl;

    CAngle cangleAB = ptA.GetAngleTo( ptB );
    CAngle cangleBA = ptB.GetAngleTo( ptA );
    cangleBA -= cangleB;
    BB_DBG(2) << "Relative Angle: (AB:" << cangleAB << ") & (BA:" << cangleBA << ")" << endl;

    relAB = calculateOrientationSegmentOPRA( m, cangleAB.get() );
    relBA = calculateOrientationSegmentOPRA( m, cangleBA.get() );

    if( ptA == ptB ) {
        result.push_back( relBA );
    } else {

        result.push_back( relAB );
        result.push_back( relBA );
    }
    return result;
}


std::vector< int > getOPRA_Relation3(int m,
                                     Point ptA, double oriA,
                                     Point ptB, double oriB) {
    int relAB, relBA;

    Point ptOrigin( 0,0 );

    std::vector< int > result;
    CAngle cangleA(oriA);
    CAngle cangleB(oriB);

    CAngle cangleAB = ptA.GetAngleTo( ptB );
    CAngle cangleBA = ptB.GetAngleTo( ptA );

    CAngle angleI = cangleAB - cangleA + cangleB;
    CAngle angleJ = cangleBA - cangleB + cangleA;

    relAB = calculateOrientationSegmentOPRA( m, angleI.get() );
    relBA = calculateOrientationSegmentOPRA( m, angleJ.get() );

    if( ptA == ptB ) {
        result.push_back( relBA );
    } else {

        result.push_back( relAB );
        result.push_back( relBA );
    }
    return result;
}




std::string getOPRA_Relation_String(int m,
                                    Point ptA, Vector vOriA,
                                    Point ptB, Vector vOriB) {
    std::vector< int > vResult = getOPRA_Relation(m, ptA, vOriA, ptB, vOriB);
    char cString[20];
    std::string sResult;
    if( vResult.size()==1 ) {
        sprintf( cString, "s_%d", vResult[0]);
        sResult = (std::string)cString;
    } else if( vResult.size()==2 ) {
        sprintf( cString, "%d_%d", vResult[0],vResult[1]);
        sResult = (std::string)cString;
    } else {
        sResult = ":error";
    }
    return sResult;
}


std::string getOPRA_Relation_String(int m,
                                    const double Ax, const double Ay, double Adx, double Ady,
                                    const double Bx, const double By, double Bdx, double Bdy) {
    std::vector< int > vResult =  getOPRA_Relation( m, Ax, Ay, Adx, Ady, Bx, By, Bdx, Bdy);
    char cString[20];
    std::string sResult;
    if( vResult.size()==1 ) {
        sprintf( cString, "s_%d", vResult[0]);
        sResult = (std::string)cString;
    } else if( vResult.size()==2 ) {
        sprintf( cString, "%d_%d", vResult[0],vResult[1]);
        sResult = (std::string)cString;
    } else {
        sResult = ":error";
    }
    return sResult;
}



std::vector< int > getOPRA_Relation(int m,
                                    const double Ax, const double Ay, double Aori,
                                    const double Bx, const double By, double Bori) {
    Point ptA(Ax, Ay);
    Point ptB(Bx, By);

    BB_DBG(3) << "Punkt A: " << ptA << ":" << Aori << "   Punkt B: " << ptB << ":" << Bori << endl;
    return getOPRA_Relation(m, ptA, Aori, ptB, Bori);
}


std::vector< int > getOPRA_Relation(int m,
                                    Point ptA, Vector vOriA,
                                    Point ptB, Vector vOriB) {
    CAngle oriA( vOriA.GetAngle() );
    CAngle oriB( vOriB.GetAngle() );
    BB_DBG(4) << "(" << ptA << ":" << vOriA << "  ==>  " << oriA << endl;
    BB_DBG(4) << "(" << ptB << ":" << vOriB << "  ==>  " << oriB << endl;
    return getOPRA_Relation(m, ptA, oriA.get(), ptB, oriB.get());
}


std::vector< int > getOPRA_Relation(int m,
                                    const double Ax, const double Ay, double Adx, double Ady,
                                    const double Bx, const double By, double Bdx, double Bdy) {
    Point ptA(Ax, Ay);
    Point ptB(Bx, By);
    Vector vA( Adx, Ady);
    Vector vB( Bdx, Bdy);
    return getOPRA_Relation(m, ptA, vA, ptB, vB);
}



/*****************************************************************************
*** GPCC
*****************************************************************************/
int getGPCC_MaxSectors( int m) {
    return ( (4*m-1)*(8*m));
}
int getGPCC_MaxRelations( int m) {
    return ( (4*m-1)*(8*m) +3 );
}

std::vector< std::vector< int > > getGPCC_All_Base_Relations( int m ) {
    std::vector< std::vector< int > > vRelation;
    std::vector< int > relation;
    relation.resize(2);
    int maxDist = 4*m-1;     // GTPCC 2*m
    int maxRot  = 8*m;       // GTPCC 4*m
    // special cases tri, dou, sam
    relation[0] = 0;
    relation[1] = 0;
    vRelation.push_back( relation );
    relation[1] = -1;
    vRelation.push_back( relation );
    relation[1] = -2;
    vRelation.push_back( relation );
    for( int i=1; i<=maxDist; i++) {
        relation[0]=i;
        for( int j=1; j<=maxRot; j++) {
            relation[1]=j;
            vRelation.push_back( relation );
        }
    }
    return vRelation;
}

std::string getGPCC_All_Base_Relations_String( int m ) {
    return relations2String_GPCC_GTPCC( getGPCC_All_Base_Relations( m ) );
}


std::string getGPCC_Relation_String(int m,
                                    Point ptOrigin,
                                    Point ptRelatum,
                                    Point ptReferent) {
    std::vector< std::vector< int > > vResult = getGPCC_Relation(m, ptOrigin, ptRelatum, ptReferent);
    return relations2String_GPCC_GTPCC(vResult);
}

std::string getGPCC_Relation_String(int m,
                                    const double originX, const double originY,
                                    const double relatumX, const double relatumY,
                                    const double referentX, const double referentY) {
    std::vector< std::vector< int > > vResult = getGPCC_Relation(m, originX, originY, relatumX, relatumY, referentX, referentY);
    return relations2String_GPCC_GTPCC(vResult);
}


std::vector< std::vector< int > > getGPCC_Relation(int m,
        Point ptOrigin,
        Point ptRelatum,
        Point ptReferent) {
    return getGPCC_Relation(m, ptOrigin.x, ptOrigin.y, ptRelatum.x,
                            ptRelatum.y, ptReferent.x, ptReferent.y);
}

std::vector< std::vector< int > > getGPCC_Relation(int m,
        const double originX, const double originY,
        const double relatumX, const double relatumY,
        const double referentX, const double referentY) {
    GPCC gpcc(m, relatumX, relatumY, originX , originY);
    return gpcc.getSectorTupel( referentX, referentY );
}

std::vector< std::vector< int > > getGPCC_CNH_Relations( int m, std::vector< int > relation ) {
    GPCC gpcc(m, 0.0, 0.0, 1.0 , 0.0);
    return gpcc.getNeighborhoodRelations( relation );
}


std::string getGPCC_CNH_Relations_String( int m, std::vector< int > relation ) {
    // correction term for orientation naming
    relation[1] += 1;
    std::vector< std::vector< int > > vResult = getGPCC_CNH_Relations( m, relation );
    return relations2String_GPCC_GTPCC(vResult);
}

std::string getGPCC_CNH_Relations_String( int m, std::string relation ) {
    // more than one '_' in string => ERROR
    // more than one relation => only take 1st, ignore rest
    std::string sRelDist, sRelOri;
    int relDist, relOri;
    std::vector< int > vRelation;
    vRelation.resize(2);
    int pos;
    if( (pos=relation.find("_", 0))==std::string::npos ) {
        // look for special cases (-1 for ori because of 0..4m-1 instead of 1..4m)
        if( relation=="tri" ) { //[0,0]
            vRelation[0] = 0;
            vRelation[1] = -1;
        } else if( relation=="dou" ) {//[0,-1]
            vRelation[0] = 0;
            vRelation[1] = -2;
        } else if( relation=="sam" ) { //[0,-2]
            vRelation[0] = 0;
            vRelation[1] = -3;
        } else {
            return ": error";
        }
    } else {
        // cout << "Position of _ at " << pos << endl;
        sRelDist = relation.substr(0, pos);
        if( relation.find("_", pos+1)!= std::string::npos )
            return ": error";
        sRelOri = relation.substr(pos+1, relation.size()-(pos+1));
        relDist = atoi( sRelDist.c_str() );
        //cout << "RelDist: " << sRelDist << endl;
        if( relDist==0 && sRelDist!="0" )
            return ": error";
        //cout << "RelOri: " << sRelOri << endl;
        relOri = atoi( sRelOri.c_str() );
        if( relOri==0 && sRelOri!="0" )
            return ": error";
        vRelation[0] = relDist;
        vRelation[1] = relOri;
    }
    return getGPCC_CNH_Relations_String( m, vRelation );
}

/*****************************************************************************
*** GTPCC
*****************************************************************************/
int getGTPCC_MaxSectors( int m) {
    return ( (4*m-1)*(8*m));
}
int getGTPCC_MaxRelations( int m) {
    return ( (4*m-1)*(8*m) +3 );
}

std::vector< std::vector< int > > getGTPCC_All_Base_Relations( int m ) {
    std::vector< std::vector< int > > vRelation;
    std::vector< int > relation;
    relation.resize(2);
    int maxDist = 2*m;     // GTPCC 2*m
    int maxRot  = 4*m;       // GTPCC 4*m
    // special cases tri, dou, sam
    relation[0] = 0;
    relation[1] = 0;
    vRelation.push_back( relation );
    relation[1] = -1;
    vRelation.push_back( relation );
    relation[1] = -2;
    vRelation.push_back( relation );
    for( int i=1; i<=maxDist; i++) {
        relation[0]=i;
        for( int j=1; j<=maxRot; j++) {
            relation[1]=j;
            vRelation.push_back( relation );
        }
    }
    return vRelation;
}

std::string getGTPCC_All_Base_Relations_String( int m ) {
    return relations2String_GPCC_GTPCC( getGTPCC_All_Base_Relations( m ) );
}

std::string getGTPCC_Relation_String(int m,
                                     Point ptOrigin,
                                     Point ptRelatum,
                                     Point ptReferent) {
    std::vector< std::vector< int > > vResult = getGTPCC_Relation(m, ptOrigin, ptRelatum, ptReferent);
    return relations2String_GPCC_GTPCC(vResult);
}

std::string getGTPCC_Relation_String(int m,
                                     const double originX, const double originY,
                                     const double relatumX, const double relatumY,
                                     const double referentX, const double referentY) {
    std::vector< std::vector< int > > vResult = getGTPCC_Relation(m, originX, originY, relatumX, relatumY, referentX, referentY);
    return relations2String_GPCC_GTPCC(vResult);
}


std::vector< std::vector< int > > getGTPCC_Relation(int m,
        Point ptOrigin,
        Point ptRelatum,
        Point ptReferent) {
    return getGTPCC_Relation(m, ptOrigin.x, ptOrigin.y, ptRelatum.x,
                             ptRelatum.y, ptReferent.x, ptReferent.y);
}

std::vector< std::vector< int > > getGTPCC_Relation(int m,
        const double originX, const double originY,
        const double relatumX, const double relatumY,
        const double referentX, const double referentY) {
    GTPCC gtpcc(m, relatumX, relatumY, originX , originY);
    return gtpcc.getSectorTupel( referentX, referentY );
}

std::vector< std::vector< int > > getGTPCC_CNH_Relations( int m, std::vector< int > relation ) {
    GTPCC gtpcc(m, 0.0, 0.0, 1.0 , 0.0);
    return gtpcc.getNeighborhoodRelations( relation );
}


std::string getGTPCC_CNH_Relations_String( int m, std::vector< int > relation ) {
    // correction term for orientation naming
    relation[1] += 1;
    // cout << "getGTPCC relation: " << relation[0] << ":" << relation[1] << endl;
    std::vector< std::vector< int > > vResult = getGTPCC_CNH_Relations( m, relation );
    return relations2String_GPCC_GTPCC(vResult);
}

std::string getGTPCC_CNH_Relations_String( int m, std::string relation ) {
    // more than one '_' in string => ERROR
    // more than one relation => only take 1st, ignore rest
    std::string sRelDist, sRelOri;
    int relDist, relOri;
    std::vector< int > vRelation;
    vRelation.resize(2);
    int pos;
    if( (pos=relation.find("_", 0))==std::string::npos ) {
        // look for special cases (-1 for ori because of 0..4m-1 instead of 1..4m)
        if( relation=="tri" ) { //[0,0]
            vRelation[0] = 0;
            vRelation[1] = -1;
        } else if( relation=="dou" ) {//[0,-1]
            vRelation[0] = 0;
            vRelation[1] = -2;
        } else if( relation=="sam" ) { //[0,-2]
            vRelation[0] = 0;
            vRelation[1] = -3;
        } else {
            return ": error";
        }
    } else {
        sRelDist = relation.substr(0, pos);
        if( relation.find("_", pos+1)!= std::string::npos )
            return ": error";
        sRelOri = relation.substr(pos+1, relation.size()-(pos+1));
        relDist = atoi( sRelDist.c_str() );
        if( relDist==0 && sRelDist!="0" )
            return ": error";
        relOri = atoi( sRelOri.c_str() );
        if( relOri==0 && sRelOri!="0" )
            return ": error";
        vRelation[0] = relDist;
        vRelation[1] = relOri;
    }
    return getGTPCC_CNH_Relations_String( m, vRelation );
}



/*****************************************************************************
*** Dipole Calculus (DC) 
*****************************************************************************/
// m = 	0	DRA  (Schlieder's origin)
// m = 	1	DRA_c  (Schlieder extended with 3 points on a line are allowed)
//	2	DRA_f  (fine grained variant with 72 basic relations)
//	3	DRA_fp (fine grained variant with parallelity => 80 basic relations)
int		getDC_MaxRelations( int m ) {
    if( m<DRA || m>DRA_fp )
        return 0;
    if( m == DRA )
        return 14;
    if( m == DRA_c )
        return 24;
    if( m == DRA_f )
        return 72;
    if( m == DRA_fp )
        return 80;
}



std::string getDC_Relation_String( int m,
                                   Point ptStartA,
                                   Point ptEndA,
                                   Point ptStartB,
                                   Point ptEndB) {
    std::vector< char > vcResult = getDC_Relation( m, ptStartA, ptEndA, ptStartB, ptEndB);
    std::string sResult;
    if( vcResult.size() < 4 ) {
        sResult = ":error";
        return sResult;
    }
    for( int i=0; i<vcResult.size(); i++) {
        sResult += vcResult[i];
    }
    return sResult;
}


std::string getDC_Relation_String(int m,
                                  float ptStartAx, float ptStartAy,
                                  float ptEndAx, float ptEndAy,
                                  float ptStartBx, float ptStartBy,
                                  float ptEndBx, float ptEndBy) {
    std::vector< char >
    vcResult = getDC_Relation(m,
                              ptStartAx, ptStartAy, ptEndAx, ptEndAy,
                              ptStartBx, ptStartBy, ptEndBx, ptEndBy);
    std::string sResult;
    if( vcResult.size() < 4 ) {
        sResult = ":error";
        return sResult;
    }
    for( int i=0; i<vcResult.size(); i++) {
        sResult += vcResult[i];
    }
    return sResult;
}

// returns list of 4 or 5 char (depending on m)
//	with char 0-3: l/r e/s b/f/i
//	with char 4  : +/-/P/A
std::vector< char > getDC_Relation(int m,
                                   float ptStartAx, float ptStartAy,
                                   float ptEndAx, float ptEndAy,
                                   float ptStartBx, float ptStartBy,
                                   float ptEndBx, float ptEndBy) {
    Point ptStartA( ptStartAx, ptStartAy);
    Point ptEndA(ptEndAx, ptEndAy);
    Point ptStartB(ptStartBx, ptStartBy);
    Point ptEndB(ptEndBx, ptEndBy);

    return getDC_Relation( m, ptStartA, ptEndA, ptStartB, ptEndB);
}

std::vector< char > getDC_Relation( int m,
                                    Point ptStartA,
                                    Point ptEndA,
                                    Point ptStartB,
                                    Point ptEndB) {
    std::vector< int > pre_result;
    std::vector< char > result;

    std::vector< Line > lines;
    std::vector< Point > points;

    Line lineA(ptStartA, ptEndA);
    Line lineB(ptStartB, ptEndB);

    lines.push_back( lineA );
    lines.push_back( lineA );
    lines.push_back( lineB );
    lines.push_back( lineB );

    points.push_back( ptStartB );
    points.push_back( ptEndB );
    points.push_back( ptStartA );
    points.push_back( ptEndA );

    pre_result.push_back( lineA.GetSide(ptStartB) );
    pre_result.push_back( lineA.GetSide(ptEndB) );
    pre_result.push_back( lineB.GetSide(ptStartA) );
    pre_result.push_back( lineB.GetSide(ptEndA) );

    // if DRA no 3 points are allowed on a line
    // DRA:	r,l
    // DRA_c:	r,l & e,s
    // DRA_f:	r,l & e,s & b,i,f
    // DRA_fp:	r,l & e,s & b,i,f + 5th position: A,P,+,-

    for( unsigned int index=0; index<4; index++) {
        // calculate relation char
        int side = lines[index].GetSide( points[index] );
        if(  side == (-1)) {
            result.push_back( 'l' );
        } else if( side == 1) {
            result.push_back( 'r' );
        } else { // =0 => on line
            if( lines[index].ptStart == points[index] )
                result.push_back( 's' );
            else if( lines[index].ptEnd == points[index] )
                result.push_back( 'e' );
            else {
                int OnLine = lines[index].PositionOnLineSegment( points[index] );
                if( lines[index].PositionOnLineSegment( points[index] ) == (-1))
                    result.push_back( 'b' );
                else if( lines[index].PositionOnLineSegment( points[index] ) == 0)
                    result.push_back( 'i' );
                else if( lines[index].PositionOnLineSegment( points[index] ) == (1))
                    result.push_back( 'f' );
                else {
                    cout << "( :error )" << endl;
                    exit(-1);
                }
            }
        }
    }
    // Restrictions for the specific Dipole cases
    if( m==DRA ) {
        for( unsigned int i=0; i<4; i++) {
            if(result[i]=='s' || result[i]=='e' || result[i]=='b' || result[i]=='i' || result[i]=='f' ) {
                result.clear();
            }
        }
    }
    if( m==DRA_c ) {
        for( unsigned int i=0; i<4; i++) {
            if( result[i]=='b' || result[i]=='i' || result[i]=='f' ) {
                result.clear();
            }
        }
    }
    //    if( m==DRA ) { // nothing to be done
    //     }


    if( m==DRA_fp ) {
        if( lineA.IsParallelTo( lineB )) {
            string str;
            for( int i=0; i<result.size();i++ ) {
                str += result[i];
                if( str=="sese" || str=="ffbb" || str=="efbs" || str=="ifbi" ||
                        str=="bfii" || str=="sfsi" || str=="beie" || str=="bbff" ||
                        str=="bsef" || str=="biif" || str=="iibf" || str=="sisf" ||
                        str=="iebe" || str=="rrll"  || str=="llrr" ) {
                    result.push_back( 'P' ); // Allen cases
                }
                if( str=="eses" || str=="ffff" || str=="fefe" || str=="fifi" ||
                        str=="fbii" || str=="fsei" || str=="ebis" || str=="iifb" ||
                        str=="eifs" || str=="iseb" || str=="bbbb" || str=="sbsb" ||
                        str=="ibib" || str=="rrrr" || str=="llll") {
                    result.push_back( 'A' ); // Converse Allen cases
                }
            }
        } else {
            //         Point ptCut;
            //         if( lineA.IsParallelTo( lineB )) {
            //             // now is the question if A or P
            //             if( lineA == lineB ) {
            //                 string str;
            //                 for( int i=0; i<result.size();i++ ) {
            //                     str += result[i];
            //                 }
            //                 result.push_back( '*' );
            //             } else {
            //                 Line lineX( ptStartA, ptStartB);
            //                 Line lineY( ptEndA, ptEndB);
            //                 if( lineX.GetCut( lineY, ptCut ) <= 0 ) { // no cut => P
            //                     result.push_back( 'A' );
            //                 } else {
            //                     result.push_back( 'P' );
            //                 }
            //             }
            //         } else {
            //             // calculate mathematical orientation
            //             if( lineB.GetAngle()-lineA.GetAngle() > 0 ) {
            //                 result.push_back( '-' );
            //             } else { // lineB.GetAngle()-lineA.GetAngle() < 0
            //                 result.push_back( '+' );
            //             }
            //         }
            // Shift all points by -sA
            Point ptOrigin( 0,0);
            //cout << "Line A: " << ptStartA << ptEndA << "  & Line B: " << ptStartB << ptEndB << endl;
            ptEndA.Shift( ptOrigin - ptStartA );
            ptStartB.Shift( ptOrigin - ptStartA );
            ptEndB.Shift( ptOrigin - ptStartA );
            ptStartA.Shift( ptOrigin - ptStartA );
            //cout << "Line A: " << ptStartA << ptEndA << "  & Line B: " << ptStartB << ptEndB << endl;
            // Rotate all points by -phi(eA)
            float angle = ptStartA.GetAngleTo( ptEndA );
            ptStartA.Rotate( -angle );
            ptEndA.Rotate( -angle );
            ptStartB.Rotate( -angle );
            ptEndB.Rotate( -angle );
            //cout << "Line A: " << ptStartA << ptEndA << "  & Line B: " << ptStartB << ptEndB << endl;
            // Shift eB by -sB
            ptEndB.Shift( ptOrigin - ptStartB );
            ptStartB.Shift( ptOrigin - ptStartB );
            //cout << "Line A: " << ptStartA << ptEndA << "  & Line B: " << ptStartB << ptEndB << endl;
            //             if(ptEndB.Y()>-0.0001 && ptEndB.Y()<0.0001) {
            //                 // lines are parallel => A or P (epsilon due to shift/rotate errors)
            //                 if( ptEndB.X()>0 ) {
            //                     result.push_back( 'P' );
            //                 } else if( ptEndB.X()<0 ) {
            //                     result.push_back( 'A' );
            //                 } else {
            //                     result.push_back( '*' );
            //                 }
            //             } else
            if(ptEndB.Y()>0) {
                result.push_back( '+' );
            } else if(ptEndB.Y()<0) {
                result.push_back( '-' );
            }
        }
    }
    return result;
}
/*
INDEX 0:
StartA == StartB	=> s
EndA == StartB 	=> e
lineA.PositionOnLineSegment( ptStartB );
 
INDEX 1:
StartA == EndB	=> s
EndA == EndB 	=> e
lineA.PositionOnLineSegment( ptEndB );
 
INDEX 2:
StartB == StartA	=> s
StartB == EndA 	=> e
lineB.PositionOnLineSegment( ptStartA );
 
 
INDEX 3:
EndB == StartA	=> s
EndB == EndA 	=> e
lineB.PositionOnLineSegment( ptEndA );
 
 
*/


/*****************************************************************************
*** ProjectionBasedAbsoluteDirectionCalculus (PBADC)
*****************************************************************************/
int getPBADC_Relation( int m, Point ptOrigin, Point ptRelatum, float angleRelative) {
    Point origin( 0,0);
    ptRelatum.Shift( origin-ptOrigin );
    ptRelatum.Rotate( -angleRelative, origin );
    BB_DBG(3) << origin << " : " << ptRelatum << endl;
    return calculateOrientationSegmentOPRA( m, origin.GetAngleTo( ptRelatum ) );
};


int getPBADC_Relation( int m, double ptOriginX, double ptOriginY, double ptRelatumX, double ptRelatumY, float angleRelative) {
    Point ptOrigin( ptOriginX, ptOriginY );
    Point ptRelatum( ptRelatumX, ptRelatumY );
    return getPBADC_Relation( m, ptOrigin, ptRelatum, angleRelative);
}
int getPBADC_Relation( Point ptOrigin, Point ptRelatum, float angleRelative) {
    int m=2;
    return getPBADC_Relation( m, ptOrigin, ptRelatum, angleRelative);
}
int getPBADC_Relation( double ptOriginX, double ptOriginY, double ptRelatumX, double ptRelatumY, float angleRelative) {
    int m=2;
    return getPBADC_Relation( ptOriginX, ptOriginY, ptRelatumX, ptRelatumY, angleRelative);
}


std::string getPBADC_Relation_String( int m, Point ptOrigin, Point ptRelatum, float angleRelative) {
    char cString[20];
    std::string sResult;
    sprintf( cString, "%d", getPBADC_Relation( m, ptOrigin, ptRelatum, angleRelative));
    return (std::string)cString;
}
std::string getPBADC_Relation_String( int m, double ptOriginX, double ptOriginY, double ptRelatumX, double ptRelatumY, float angleRelative) {
    char cString[20];
    std::string sResult;
    sprintf( cString, "%d", getPBADC_Relation( m, ptOriginX, ptOriginY, ptRelatumX, ptRelatumY, angleRelative));
    return (std::string)cString;
}
std::string getPBADC_Relation_String( Point ptOrigin, Point ptRelatum, float angleRelative) {
    char cString[20];
    std::string sResult;
    sprintf( cString, "%d", getPBADC_Relation( ptOrigin, ptRelatum, angleRelative));
    return (std::string)cString;
}
std::string getPBADC_Relation_String( double ptOriginX, double ptOriginY, double ptRelatumX, double ptRelatumY, float angleRelative) {
    char cString[20];
    std::string sResult;
    sprintf( cString, "%d", getPBADC_Relation( ptOriginX, ptOriginY, ptRelatumX, ptRelatumY, angleRelative));
    return (std::string)cString;
}


/*****************************************************************************
*** Single-Cross Calculus (SCC) 
*****************************************************************************/
std::string getSCC_Relation_String( int m, Point ptOrigin, Point ptRelatum, Point ptReferent) {
  if(ptOrigin == ptRelatum) {
    // return ":error";
    // dou/tri not included in the original definition
    if(ptOrigin == ptReferent) {
      return "tri";      
    } else {
      return "dou";      
    }
  }
  // Point origin( 0,0 );
  
  CAngle angle;
  char cString[20];
  std::string sResult;
  // Teil 1: A,B ? C
  if( ptRelatum == ptReferent) {
    return (std::string)"b";
    // sprintf( cString, "%c\0", 'b');
  } else {
    Point c( ptReferent );
    angle = -ptOrigin.GetAngleTo( ptRelatum );
    c.Rotate( angle.get(), ptRelatum );
    sprintf( cString, "%d\0", calculateOrientationSegmentOPRA( 2, ptRelatum.GetAngleTo( c ) ));
  }
  sResult += cString;
  return sResult;
}

std::string getSCC_Relation_String( Point ptOrigin, Point ptRelatum, Point ptReferent) {
  int m=2;
  return getSCC_Relation_String( m, ptOrigin, ptRelatum, ptReferent);
}

std::string getSCC_Relation_String( int m,
                                    double ptOriginX, double ptOriginY,
                                    double ptRelatumX, double ptRelatumY,
                                    double ptReferentX, double ptReferentY) {
    Point ptOrigin( ptOriginX, ptOriginY);
    Point ptRelatum( ptRelatumX, ptRelatumY);
    Point ptReferent( ptReferentX, ptReferentY);
    BB_DBG(3) << ptOrigin << ptReferent << ptRelatum << endl;
    return getSCC_Relation_String( m, ptOrigin, ptRelatum, ptReferent);
}

const char* qualify_scc( double ptOriginX, double ptOriginY,
			 double ptRelatumX, double ptRelatumY,
			 double ptReferentX, double ptReferentY) {
  char res[1024];
  sprintf(res,"%s", getSCC_Relation_String( ptOriginX, ptOriginY, ptRelatumX, ptRelatumY, ptReferentX, ptReferentY).c_str());
  for (unsigned int i=0; i<strlen(res); i++){
    cRes[i] = res[i];
  }
  return cRes;
}

std::string getSCC_Relation_String( double ptOriginX, double ptOriginY,
                                    double ptRelatumX, double ptRelatumY,
                                    double ptReferentX, double ptReferentY) {
    int m=2;
    return getSCC_Relation_String( m, ptOriginX, ptOriginY, ptRelatumX, ptRelatumY, ptReferentX, ptReferentY);
}


/*****************************************************************************
*** Double-Cross Calculus Single Number Notation (DCC2) 
*****************************************************************************/
std::string transferDCCtoDCC2( std::string relation ) {
  //cout << "transferDCCtoDCC2 ENTER" << endl;
  std::string sResult = ":error";
  // 0_4 1_5 2_5 3_5 3_6 3_7 4_0 4_4 4_A 5_1 5_2 5_3 6_3 7_3 B_4 DOU TRI
  if( relation=="dou" || relation=="Dou" || relation=="DOU" ||
      relation=="tri" || relation=="Tri" || relation=="TRI" ) {
    return relation;
  }
  if( relation=="0_4" ) {
    return "0";
  }
  if( relation=="1_5" ) {
    return "1";
  }
  if( relation=="2_5" ) {
    return "2";
  }
  if( relation=="3_5" ) {
    return "3";
  }
  if( relation=="3_6" ) {
    return "4";
  }
  if( relation=="3_7" ) {
    return "5";
  }
  if( relation=="4_0" ) {
    return "6";
  }
  if( relation=="5_1" ) {
    return "7";
  }
  if( relation=="5_2" ) {
    return "8";
  }
  if( relation=="5_3" ) {
    return "9";
  }
  if( relation=="6_3" ) {
    return "10";
  }
  if( relation=="7_3" ) {
    return "11";
  }
  if( relation=="4_4" ) {
    return "12";
  }
  if( relation=="4_a" || relation=="4_A" ) {
    return "A";
  }
  if( relation=="b_4" || relation=="B_4" ) {
    return "B";
  }
  return sResult;
}


std::string getDCC2_Relation_String( Point ptOrigin, Point ptRelatum, Point ptReferent) {
  //cout << "getDCC2_Relation_String (pt,pt,pt)" << endl;
  return transferDCCtoDCC2( getDCC_Relation_String( ptOrigin, ptRelatum, ptReferent) );
}

std::string getDCC2_Relation_String( double ptOriginX, double ptOriginY,
				     double ptRelatumX, double ptRelatumY,
				     double ptReferentX, double ptReferentY) {
  //cout << "getDCC2_Relation_String (x1,y1,...,x3,y3)" << endl;
  return transferDCCtoDCC2( getDCC_Relation_String( ptOriginX, ptOriginY, 
						     ptRelatumX, ptRelatumY,
						     ptReferentX, ptReferentY));
}

// free scalable variants of DCC make no sense here with this notation


/*****************************************************************************
*** Double-Cross Calculus (DCC) 
*****************************************************************************/
std::string getDCC_Relation_String( int m, Point ptOrigin, Point ptRelatum, Point ptReferent) {

    CAngle angle;
    char cString[20];
    std::string sResult;
    // Now orientation given if origin == relatum => i (following [Fre92])
    if( ptOrigin == ptRelatum) {
      if( ptRelatum == ptReferent) {
        return (std::string)"tri"; // A = B = C => 'tri'
      } else {
	return (std::string)"dou"; // A = B != C => 'dou'
      }
      // sprintf( cString, "%c\0", 'b');
    }
    // Teil 1: A,B ? C
    if( ptRelatum == ptReferent) {
        return (std::string)"b_4";
        // sprintf( cString, "%c\0", 'b');
    } else {
        Point c( ptReferent );
        angle = -ptOrigin.GetAngleTo( ptRelatum );
        c.Rotate( angle.get(), ptRelatum );
        sprintf( cString, "%d_\0", calculateOrientationSegmentOPRA( m, ptRelatum.GetAngleTo( c ) ));
    }
    sResult += cString;

    // Teil 2: B,A ? C
    if( ptOrigin == ptReferent) {
        // sprintf( cString, "%c\0", 'a');
        return (std::string)"4_a";
    } else {
        Point c( ptReferent );
        angle = -ptRelatum.GetAngleTo( ptOrigin );
        c.Rotate( angle.get(), ptOrigin );
        sprintf( cString, "%d\0", calculateOrientationSegmentOPRA( m, ptOrigin.GetAngleTo( c ) ));
    }
    sResult += cString;
    return sResult;
}

std::string getDCC_Relation_String( Point ptOrigin, Point ptRelatum, Point ptReferent) {
    int m=2;
    return getDCC_Relation_String( m, ptOrigin, ptRelatum, ptReferent);
}

std::string getDCC_Relation_String( int m,
                                    double ptOriginX, double ptOriginY,
                                    double ptRelatumX, double ptRelatumY,
                                    double ptReferentX, double ptReferentY) {
    Point ptOrigin( ptOriginX, ptOriginY);
    Point ptRelatum( ptRelatumX, ptRelatumY);
    Point ptReferent( ptReferentX, ptReferentY);
    BB_DBG(3) << ptOrigin << ptReferent << ptRelatum << endl;
    return getDCC_Relation_String( m, ptOrigin, ptRelatum, ptReferent);
}
std::string getDCC_Relation_String( double ptOriginX, double ptOriginY,
                                    double ptRelatumX, double ptRelatumY,
                                    double ptReferentX, double ptReferentY) {
    int m=2;
    return getDCC_Relation_String( m, ptOriginX, ptOriginY, ptRelatumX, ptRelatumY, ptReferentX, ptReferentY);
}


/*****************************************************************************
*** Ternary Point Configuration Calculus (TPCC) 
*****************************************************************************/
std::string getTPCC_Relation_String( Point ptOrigin, Point ptRelatum, Point ptReferent) {
    std::string sResult;
    std::string sDist;
    // TPCC is a special case of GPCC
    // resolve special cases although handled in GPCC as well
    if( ptOrigin==ptRelatum ) {
        if( ptOrigin==ptReferent ) {
            return "tri";
        } else {
            return "dou";
        }
    }
    float distBC = ptRelatum.GetDistanceTo( ptReferent );
    if ( distBC == 0 ) {
        return "sam";
    }

    GTPCC gtpcc(2, ptRelatum.X(), ptRelatum.Y(), ptOrigin.X() , ptOrigin.Y());
    gtpcc.setLinear( true );
    // now linear segment boarders (NOT as disjunction of neighboring relations)
    // distance = 0,1 or 2 with 1 is close and 2 is distant
    // orientation
    std::vector< std::vector< int > > vResult = gtpcc.getSectorTupel( ptReferent.X(), ptReferent.Y() );

    // close or distant ? (distance ist unique)
    if( vResult[0][0]==0 ) {
        sDist = "0";
    } else if( vResult[0][0]==1 ) {
        sDist = "c";
    } else {
        sDist = "d";
    }
    // size=1 or 2
    if( vResult.size() == 1 ) {
        // which angle segment?
        switch ( vResult[0][1]-1 ) {
        case 0:
            sResult = sDist + "sb";
            break;
        case 1:
            sResult = sDist + "lb";
            break;
        case 2:
            sResult = "(" + sDist + "lb" + "," + sDist + "bl" +")";
            break;
        case 3:
            sResult = sDist + "bl";
            break;
        case 4:
            sResult = sDist + "sl";
            break;
        case 5:
            sResult = sDist + "fl";
            break;
        case 6:
            sResult = "(" + sDist + "fl" + "," + sDist + "lf" +")";
            break;
        case 7:
            sResult = sDist + "lf";
            break;
        case 8:
            sResult = sDist + "sf";
            break;
        case 9:
            sResult = sDist + "rf";
            break;
        case 10:
            sResult = "(" + sDist + "rf" + "," + sDist + "fr" +")";
            break;
        case 11:
            sResult = sDist + "fr";
            break;
        case 12:
            sResult = sDist + "sr";
            break;
        case 13:
            sResult = sDist + "br";
            break;
        case 14:
            sResult = "(" + sDist + "br" + "," + sDist + "rb" +")";
            break;
        case 15:
            sResult = sDist + "rb";
            break;
        default: // ERROR
            sResult = sDist + "XX";
            break;
        }
    } else { // if( vResult.size > 1 )
        sResult = "???"; // ERROR
    }
    //     float distAB ptOrigin.GetDistanceTo( ptRelatum );
    //     float rABC = distBC / distAB;
    //     if( rABC < 1) {
    //         sResult = "c";
    //     } else {
    //         sResult = "d";
    //     }
    //     // Angle (2./3. letter)
    //     CAngle angle = -ptOrigin.GetAngleTo( ptRelatum );
    //     ptReferent.Rotate( angle.get(), ptRelatum );
    //     int iSegmentNo = ptRelatum.GetAngleTo( ptReferent ) ;

    return sResult;
}



std::string getTPCC_Relation_String( double ptOriginX, double ptOriginY,
                                     double ptRelatumX, double ptRelatumY,
                                     double ptReferentX, double ptReferentY) {
    Point ptOrigin( ptOriginX, ptOriginY);
    Point ptRelatum( ptRelatumX, ptRelatumY);
    Point ptReferent( ptReferentX, ptReferentY);
    BB_DBG(3) << ptOrigin << ptReferent << ptRelatum << endl;
    return getTPCC_Relation_String( ptOrigin, ptRelatum, ptReferent);
}


std::string getGTPCC_All_Base_Relations_String( ){
 return "(tri dou sam csb dsb clb dlb cbl dbl csl dsl cfl dfl clf dlf csf dsf crf drf cfr dfr csr dsr cbr dbr crb drb)";
}


const char* qualify_tpcc( double ptOriginX, double ptOriginY,
			 double ptRelatumX, double ptRelatumY,
			 double ptReferentX, double ptReferentY) {
  char res[1024];
  sprintf(res,"%s", getTPCC_Relation_String( ptOriginX, ptOriginY, ptRelatumX, ptRelatumY, ptReferentX, ptReferentY).c_str());
  for (unsigned int i=0; i<strlen(res); i++){
    cRes[i] = res[i];
  }
  return cRes;
}





/*****************************************************************************
*** Qualitative Trajectory Calculus (QTC) - by vdWeghe
*****************************************************************************/
std::string getQTC_Relation_String( double pt_K1_X, double pt_K1_Y,
                                    double pt_K2_X, double pt_K2_Y,
                                    double pt_L1_X, double pt_L1_Y,
                                    double pt_L2_X, double pt_L2_Y){
   Point pt_K1( pt_K1_X, pt_K1_Y );
   Point pt_K2( pt_K2_X, pt_K2_Y );
   Point pt_L1( pt_L1_X, pt_L1_Y );
   Point pt_L2( pt_L2_X, pt_L2_Y );
   return getQTC_Relation_String( pt_K1, pt_K2, pt_L1, pt_L2);
};

std::string getQTC_Relation_String( Point pt_K1, Point pt_K2, Point pt_L1, Point pt_L2){	
   std::string sResult;
   std::vector< int > relation_C13;
   std::vector< int > relation_C24;
   // Result: [Rel_K1_K2, Rel_K2_K1]
   relation_C13 = getOPRA_Relation(2, pt_K1, pt_K1.GetAngleTo(pt_L1), pt_K2, pt_K2.GetAngleTo(pt_K1));
   // Result: [Rel_L1_L2, Rel_L2_L1]
   relation_C24 = getOPRA_Relation(2, pt_L1, pt_K1.GetAngleTo(pt_L1), pt_K2, pt_L2.GetAngleTo(pt_L1));
   // C1: K with respect to L1 (front/back)
   if( relation_C13[0]==0 || relation_C13[0]==1 || relation_C13[0]==7 ){ sResult += "-";}
   else if( relation_C13[0]==2 || relation_C13[0]==6 ){ sResult += "0";}
   else if( relation_C13[0]==3 || relation_C13[0]==4 || relation_C13[0]==5 ){ sResult += "+";}
   else { cout << "PROBLEMS C1" << endl;}
   // C2: L with respect to K1 (front/back)
   if( relation_C24[0]==0 || relation_C24[0]==1 || relation_C24[0]==7 ){ sResult += "-";}
   else if( relation_C24[0]==2 || relation_C24[0]==6 ){ sResult += "0";}
   else if( relation_C24[0]==3 || relation_C24[0]==4 || relation_C24[0]==5 ){ sResult += "+";}
   else { cout << "PROBLEMS C2" << endl;}
   // C3: K with respect to K1/L1 (left/right)
   if( relation_C13[0]==1 || relation_C13[0]==2 || relation_C13[0]==3 ){ sResult += "-";}
   else if( relation_C13[0]==0 || relation_C13[0]==4 ){ sResult += "0";}
   else if( relation_C13[0]==5 || relation_C13[0]==6 || relation_C13[0]==7 ){ sResult += "+";}
   else { cout << "PROBLEMS C3" << endl;}
   // C4: L with respect to L1/¬K1 (left/right)
   if( relation_C24[0]==1 || relation_C24[0]==2 || relation_C24[0]==3 ){ sResult += "-";}
   else if( relation_C24[0]==0 || relation_C24[0]==4 ){ sResult += "0";}
   else if( relation_C24[0]==5 || relation_C24[0]==6 || relation_C24[0]==7 ){ sResult += "+";}
   else { cout << "PROBLEMS C4" << endl;}
   return sResult;
}


char getSymbol( int val ){
   char c;
   if( val<0 || val>=3 ) return 'X';
   switch( val ){
      case 0:
         c = '-';
         break;
      case 1:
         c = '0';
         break;
      case 2:
         c = '+';
         break;
      default:
         c = 'Y';
         break;
   }
   // cout << "Symbol: " << c << endl;
   return c;
}


std::string getQTC_All_Base_Relations_String( ){
   std::string sResult;
   std::string sTmp;
   sResult = "(";
   for( int i=0; i<3; i++ ){
      for( int j=0; j<3; j++ ){
         for( int k=0; k<3; k++ ){
            for( int l=0; l<3; l++ ){
//                cout << "???????: " << getSymbol( i ) << endl;
//                cout << "???????: " << getSymbol( j ) << endl;
//                cout << "???????: " << getSymbol( k ) << endl;
//                cout << "???????: " << getSymbol( l ) << endl;
//                sTmp = getSymbol( i ) + getSymbol( j ) + getSymbol( k ) + getSymbol( l ) + " ";
//                cout << "Relation: " << sTmp << endl;
               sResult += getSymbol( i );
               sResult += getSymbol( j );
               sResult += getSymbol( k );
               sResult += getSymbol( l );
               sResult += " ";
            }
         }
      }
   }
   sResult += ")";
   return sResult;
}



/*****************************************************************************
*** TOOLS for OPRA orientation segment calculation
*****************************************************************************/
int calculateOrientationSegmentOPRA( int m, double angle ) {
    BB_DBG(4) << "calculateOrientationSegmentOPRA Entry" << endl;
    int result;
    int max = getOPRA_MaxSectors( m );
    BB_DBG(7) << "max = " << max << " & m = " << m << endl;
    CAngle lb( 0.0 );
    CAngle hb( 0.0 );
    CAngle cangle( angle );
    // From 0 to 2PI
    for( unsigned int iSegNo=0; iSegNo<max; iSegNo+=2 ) {
        BB_DBG(7) << "iSegNo=" << iSegNo << " < max=" << max << endl;
        lb = hb;
        if((iSegNo+2)==max) {
            hb.set( 0.0 );
        } else {
            hb.set( ((double)((iSegNo+2)*2.0*M_PI))/(double)max );
        }
        BB_DBG(6) << "lb: " << lb << "   hb: " << hb << endl;

        BB_DBG(5) << iSegNo << " (low boarder): " << lb << " == " << cangle << " ??";
	if( lb==cangle ) {
            BB_DBG(5) << "  YES  " << endl;
            return iSegNo;
        } else {
	  /*
	  printf( "\n ** lb     = %d** \n",lb.get());
	  printf( " ** cangle = %d** \n",cangle.get());
	  printf( " **          -------\n");
	  printf( " ** Diff   = %d** \n",(lb.get()-cangle.get()));
	  cout << endl << " ** lb     = " << lb << " ** "<< endl;
	  cout << " ** cangle = " << cangle << " **" << endl;
	  cout << " **          -------" << endl;
	  cout << " ** Diff   = " << lb.diff(cangle) << " **" << endl;*/

	  // workaround for linear case with M_PI
	  // if( lb==M_PI && abs(lb.diff(cangle))<0.000000000001){
	  if( lb==M_PI && abs(lb.diff(cangle))<0.000001){
	    BB_DBG(5) << "  YES  " << endl;
	    return iSegNo;
	  } else {
            BB_DBG(5) << "  NO  " << endl;
	  }
        }

        BB_DBG(5) << iSegNo+2 << " (high boarder): " << hb << " == " << cangle << " ?? ";
        if( hb==cangle) {
            BB_DBG(5) << "  YES  " << endl;
            return iSegNo+2;
        } else {
            BB_DBG(5) << "  NO  " << endl;
        }

        BB_DBG(5) << iSegNo+1 << " (between boarders): " << lb << " < " << cangle << " < " << hb << " ?? ";
        if( (lb < cangle) && (cangle < hb) ) {
            BB_DBG(5) << "  YES  " << endl;
            return iSegNo+1;
        } else {
            BB_DBG(5) << "  NO  " << endl;
        }
    }
    BB_DBG(4) << "calculateOrientationSegmentOPRA Exit (with error)" << endl;
    return -1;
}


int calculateOrientationSegmentOPRA2( int m, double angle ) {
    BB_DBG(4) << "calculateOrientationSegmentOPRA Entry" << endl;
    int result;
    int max = getOPRA_MaxSectors( m )/2;
    m=m/2;
    BB_DBG(7) << "max = " << max << " & m = " << m << endl;
    CAngle lb( 0.0 );
    CAngle hb( 0.0 );
    CAngle cangle( angle );
    // From 0 to 2PI
    for( unsigned int iSegNo=0; iSegNo<max; iSegNo+=1 ) {
        BB_DBG(7) << "iSegNo=" << iSegNo << " < max=" << max << endl;
        lb = hb;
        BB_DBG(7) << " ... 1" << endl;
        hb.set( ((double)(iSegNo+1)*M_PI)/(2.0*(double)m) );
        BB_DBG(7) << " ... 2" << endl;
        if(iSegNo==max-1)
            hb.set( 0.0 );
        BB_DBG(6) << "lb: " << lb << "   hb: " << hb << endl;
        BB_DBG(5) << 2*(iSegNo) << ": " << lb << " == " << cangle << " ??";
        if( lb == cangle) {
            BB_DBG(5) << "  YES  " << endl;
            result = 2*(iSegNo);
            break;
        } else {
            BB_DBG(5) << "  NO  " << endl;
        }
        BB_DBG(5) << 2*(iSegNo)+1 << ": " << lb << " < " << cangle << " < " << hb << " ?? ";
        if( (lb < cangle) && (cangle < hb) ) {
            BB_DBG(5) << "  YES  " << endl;
            result = 2*(iSegNo)+1;
            break;
        } else {
            BB_DBG(5) << "  NO  " << endl;
        }
        BB_DBG(7) << "iSegNo=" << iSegNo << " < max=" << max << endl;
    }
    BB_DBG(4) << "calculateOrientationSegmentOPRA Exit" << endl;
    return result;
}


// float Round(float Value, int NumPlaces) {
//     int k, Temp;
//     float Factor;
//     Factor = 1;
//     for (k = 0; k < NumPlaces; k++)
//         Factor = Factor * 10;
//     Temp = Value * Factor + 0.5;
//     return Temp / Factor;
// }

/*****************************************************************************
*** TOOL for converting G(T)PCC relation structure to string
*****************************************************************************/
std::string relations2String_GPCC_GTPCC( std::vector< std::vector < int > > vRelations) {
    std::string sResult;
    char cString[20];
    if( vRelations.size() == 0 ) {
        sResult = ":error";
        return sResult;
    }
    if( vRelations.size() > 1) {
        sResult += "(";
    }
    for( int i=0; i< vRelations.size(); i++) {
        if( vRelations[i].size() != 2 ) {
            cout << "Relation has more than 2 items ... ERROR => EXIT!" << endl;
            sResult = ":error";
            return sResult;
        }

        if (vRelations[i][0] == 0) {
            if (vRelations[i][1] == -2)
                sResult += "sam";
            if (vRelations[i][1] == -1)
                sResult += "dou";
            if (vRelations[i][1] == 0)
                sResult += "tri";
        } else {
            sprintf( cString, "%d_%d\0", vRelations[i][0], vRelations[i][1]-1);
            sResult += cString;
        }

        if( vRelations.size()>1 && i!=vRelations.size()-1) {
            sResult += " ";
        }
    }
    if( vRelations.size() > 1) {
        sResult += ")";
    }
    return sResult;
}


#endif
