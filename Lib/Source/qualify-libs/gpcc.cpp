/* This file is part of SparQ, a toolbox for qualitative spatial reasoning.
   Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
   More info at http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

  SparQ is free software and has been released under the terms of the GNU
  General Public License version 3 or later. You should have received a
  copy of the GNU General Public License along with this program. If not,
  see <http://www.gnu.org/licenses/>.
*/


// INCLUDES:
#include <sstream>
//#include <utils/utils.h>
#include <point.h>
//#include <geometry/geometry.h>
//#include <geometry/ellipse.h>
//#include <geometry/line.h>
#include <cangle.h>

#include "gpcc.h"

using namespace std;

// =================================================================================
// CONSTRUCTORs & Destructor
// =================================================================================
GPCC::GPCC( int m,
            float relatumX, float relatumY,
            float originX=0, float originY=0) {
    GPCCConstruct( m,
                   relatumX, relatumY,
                   originX , originY );
}

GPCC::GPCC( int m ) {
    // Intialize GPCC with Origin = [-1,0] and Relatum = [0,0]
    GPCCConstruct( m, 0.0, 0.0, -1.0, 0.0);
}


GPCC::GPCC() {
    // Initialize GPCC with default scale = 2
    GPCCConstruct( 2, 0.0, 0.0, -1.0, 0.0 );
}

void GPCC::GPCCConstruct( int m,
                          float relatumX, float relatumY,
                          float originX=0, float originY=0 ) {
    BB_DBG(5) << "GPCC (Constructor): Entering construction ..." << endl;
    Init();
    m_iM      = m;
    setOrigin(  originX , originY);
    setRelatum( relatumX, relatumY);
    m_bIsLinear = true;

    BB_DBG(6) << "GPCC (Constructor): " << endl
    << "Scale m = " << m_iM << endl
    << "Using Relatum ("
    << relatumX << ", " << relatumY
    << ") and Origin: "
    << originX << ", " << originY << ")" << endl;
    BB_DBG(6) << "GPCC (Constructor): Init Phase ..." << endl;
}

// =================================================================================
// PUBLIC METHODS
// =================================================================================

// =================================================================================
// SCALE
// =================================================================================
int GPCC::getScale() {
    return m_iM;
}
int GPCC::setScale(int m) {
    if( m < 1 )
        return false;
    m_iM = m;
    return true;
}

// =================================================================================
// SET/GET ORIGIN
// =================================================================================
int GPCC::setOrigin( float x, float y) {
    m_fOriginX = x;
    m_fOriginY = y;
    m_bOriginSet = true;
    m_ptOrigin.SetXY(x,y);
    BB_DBG(9) << "Origin set to : " << m_ptOrigin << endl;
    return true;
}
int GPCC::setOrigin( Point ptOrigin ) {
    m_ptOrigin = ptOrigin;
    BB_DBG(9) << "Origin set to : " << m_ptOrigin << endl;
    return true;
}
int GPCC::getOrigin( float &x, float &y) {
    if( !m_bOriginSet )
        return false;
    x = m_fOriginX;
    y = m_fOriginY;
    return true;
}

Point GPCC::getOrigin( ) {
    return m_ptOrigin;
}

// =================================================================================
// SET/GET ORIGIN
// =================================================================================
int GPCC::setRelatum( float x, float y) {
    m_fRelatumX = x;
    m_fRelatumY = y;
    m_bRelatumSet = true;
    m_ptRelatum.SetXY(x,y);
    BB_DBG(9) << "Relatum set to : " << m_ptRelatum << endl;
    return true;
}

int GPCC::setRelatum( Point ptRelatum ) {
    m_ptRelatum = ptRelatum;
    BB_DBG(9) << "Relatum set to : " << m_ptRelatum << endl;
    return true;
}

int GPCC::getRelatum( float &x, float &y) {
    if( !m_bRelatumSet )
        return false;
    x = m_fRelatumX;
    y = m_fRelatumY;
    return true;
}

Point GPCC::getRelatum( ) {
    return m_ptRelatum;
}

// =================================================================================
// Methods for Switching Linear/Planar Mode
// =================================================================================
bool GPCC::isLinear() {
    return m_bIsLinear;
}

void GPCC::setLinear( bool bLinear ) {
    m_bIsLinear = bLinear;
}

// =================================================================================
// GET maximum number of sectors
// =================================================================================
int GPCC::getMaxSectors() {  // GPCC is jepd
    if( m_iM > 0 ) {
        return ((4*m_iM)*(8*m_iM)+3);  // number of base relations with special cases
    }
    return 0;
}

int GPCC::getMaxSectors( int m ) {
    if( m > 0 ) {
        return ((4*m)*(8*m)+3);
    }
    return (-1);
}


// =================================================================================
// GET Sector No for a specific Relatum
// =================================================================================
/* Get [distance, angle] tupel for relatum [x,y]
 */
std::vector< std::vector< int > >  GPCC::getSectorTupel( float x, float y ) {
    Point ptReferent( x,y );
    return getSectorTupel( ptReferent );
}

std::vector< std::vector< int > >  GPCC::getSectorTupel( Point referent ) {
    std::vector< int > vDistanceSegments;
    std::vector< int > vOrientationSegments;
    std::vector< int > vTmp;
    std::vector< std::vector< int > > vResult;
    // Special Cases (sam, dou & tri) if Ori, Rel, and Ref are pairwise equal
    /* tri : m_ptOrigin == m_ptRelatum == referent     => [0, 0]
       dou : m_ptOrigin == m_ptRelatum != referent     => [0, -1]
       sam : m_ptOrigin != m_ptRelatum == referent     => [0, -2]
       else                                            => [p, q]
       sam : relatum == referent
       dou : origin  == relatum && referent != origin
       tri : origin  == relatum && referent == origin
    */
    vTmp.clear();
    if( m_ptRelatum == referent ) {
        if( m_ptRelatum == m_ptOrigin ) { // ori == rel == ref
            vTmp.push_back( 0 );
            vTmp.push_back( 0 );
            vResult.push_back( vTmp );
        } else {                             // ori != rel == ref
            vTmp.push_back( 0 );
            vTmp.push_back( -2 );
            vResult.push_back( vTmp );
        }
        return vResult;
    } else {
        if( m_ptRelatum == m_ptOrigin ) { // ori == rel != ref
            vTmp.push_back( 0 );
            vTmp.push_back( -1 );
            vResult.push_back( vTmp );
            return vResult;
        }
    }

    calculateRadii( referent );
    calculateTheta( referent );

    if( !m_bIsLinear ) { // GPCC
        vDistanceSegments = calculateDistanceSegment();
        vOrientationSegments = calculateOrientationSegment();
    } else { // for OPRA or TPCC
        vDistanceSegments = calculateDistanceSegmentLinear();
        vOrientationSegments = calculateOrientationSegmentLinear();
    }

    BB_DBG(4) << "Distance Segments: ";
    for( unsigned int i=0; i<vDistanceSegments.size(); i++) {
        BB_DBG(4) << vDistanceSegments[i] << " || ";
    }
    BB_DBG(4) << endl;
    BB_DBG(4) << "Orientation Segments: ";
    for( unsigned int i=0; i<vOrientationSegments.size(); i++) {
        BB_DBG(4) << vOrientationSegments[i] << " || ";
    }
    BB_DBG(4) << endl;
    // Fill result vector m_GPCC_Result
    for( unsigned int i=0; i<vDistanceSegments.size(); i++) {
        for( unsigned int j=0; j<vOrientationSegments.size(); j++) {
            vTmp.clear();
            vTmp.push_back( vDistanceSegments[i] );
            vTmp.push_back( vOrientationSegments[j] );
            vResult.push_back(vTmp);
            /* FOR DBG Purpose only
            BB_DBG(5) << endl << "vTmp : ";
            for( int k=0; k<vTmp.size(); k++) BB_DBG(5) << vTmp[k] << " ";
            BB_DBG(5) << endl << "vResult : ";
            for( int k=0; k<vResult.size(); k++){
            for( int l=0; l<vResult[k].size(); l++){
             BB_DBG(5) << "(" << k << ":" << l << ") "
                << vResult[k][l] << " || ";
            }
            }
            BB_DBG(5) << endl;
            */
        }
    }

    return vResult;
}


std::vector< std::vector< int > > GPCC::getNeighborhoodRelations( std::vector< int > relation ) {
    std::vector< std::vector< int > > result;
    std::vector< int > cnh_relation;

    int maxDist = 4*m_iM-1;                // GPCC [0..4m-1]
    int maxRot  = 8*m_iM;                // GPCC [1..8m]

    cnh_relation.resize(2);
    if( relation.size() != 2 ) {
        return result;
    }
    // Distance boarders [0..2m]                    GPCC [0..4m-1]
    if( (relation[0]<0) || (relation[0]>maxDist)) {
        //cout << "ERROR 1" << endl;
        return result;
    }
    if( relation[0]==0 ) {
        // cout << "[" << relation[0] << "_" << relation[1] << "]_" << m_iM << endl;
        if( (relation[1]!=-2) && (relation[1]!=-1) && (relation[1]!=0) ) {
            //cout << "ERROR 2" << endl;
            return result;
        }
    }
    // Rotation [1..4m]                             GPCC [1..8m]
    else if( (relation[1]<1) || (relation[1]>maxRot) ) {
        //cout << "ERROR 3" << endl;
        return result;
    }
    // Special cases sam=[0,-2]; dou=[0,-1]; tri=[0,0]
    // dsf: GTPCC={[m,2m],[m,2m+1],[m+1,2m],[m+1,2m+1]}      GPCC=[2m,4m+1]
    // tri -> dou, sam, (ori=ref)
    if( (relation[0]==0) && (relation[1]==0)) {
        // cout << "Case: tri" << endl;
        cnh_relation[0]=0;
        cnh_relation[1]=-1;
        result.push_back( cnh_relation );
        cnh_relation[0]=0;
        cnh_relation[1]=-2;
        result.push_back( cnh_relation );
        cnh_relation[0]=2*m_iM-1;
        cnh_relation[1]=4*m_iM+1;
        result.push_back( cnh_relation );
        return result;
    }
    // dou -> tri, [2m,{1..4m}]        GPCC [4m-1,{1..8m}]
    if( (relation[0]==0) && (relation[1]==-1)) {
        // cout << "Case: dou" << endl;
        cnh_relation[0]=0;
        cnh_relation[1]=0;
        result.push_back( cnh_relation );
        cnh_relation[0]=maxDist;
        for( int i=1; i<=maxRot; i++ ) {
            cnh_relation[1]=i;
            result.push_back( cnh_relation );
        }
        return result;
    }
    // sam -> tri, [0,{1..4m}]         GPCC [0,{1..8m}]
    if( (relation[0]==0) && (relation[1]==-2)) {
        // cout << "Case: sam" << endl;
        cnh_relation[0]=0;
        cnh_relation[1]=0;
        result.push_back( cnh_relation );
        cnh_relation[0]=1;
        for( int i=1; i<=maxRot; i++ ) {
            cnh_relation[1]=i;
            result.push_back( cnh_relation );
        }
        return result;
    }
    // 8 cases except dist relation is equal to one of the boarders (0 or 2m)    GPCC:(0 or 4m-1)
    // rotation boarders: (1..4m)          GPCC:(1..8m)
   // Distance-1
    if( relation[0] == 1 ) {
        cnh_relation[0]=0;
        cnh_relation[1]=-2;
        result.push_back( cnh_relation );
    } // else if( relation[0] == 0 ) {}  ... dealt with above in tri/dou/sam
    else if( relation[0] > 1 ) {
        // cout << "Case: general" << endl;
        cnh_relation[0]=relation[0]-1;
        if( relation[1]==1 ) {
            cnh_relation[1]=maxRot;
        } else {
            cnh_relation[1]=relation[1]-1;
        }
        result.push_back( cnh_relation );
        cnh_relation[1]=relation[1];
        result.push_back( cnh_relation );
        if( relation[1]==maxRot ) {
            cnh_relation[1]=1;
        } else {
            cnh_relation[1]=relation[1]+1;
        }
        result.push_back( cnh_relation );
    }
    // same Distance
    cnh_relation[0]=relation[0];
    if( relation[1]==1 ) {
        cnh_relation[1]=maxRot;
    } else {
        cnh_relation[1]=relation[1]-1;
    }
    result.push_back( cnh_relation );
    if( relation[1]==maxRot ) {
        cnh_relation[1]=1;
    } else {
        cnh_relation[1]=relation[1]+1;
    }
    result.push_back( cnh_relation );
    if( relation[0] < maxDist ) {
        cnh_relation[0]=relation[0]+1;
        if( relation[1]==1 ) {
            cnh_relation[1]=maxRot;
        } else {
            cnh_relation[1]=relation[1]-1;
        }
        result.push_back( cnh_relation );
        cnh_relation[1]=relation[1];
        result.push_back( cnh_relation );
        if( relation[1]==maxRot ) {
            cnh_relation[1]=1;
        } else {
            cnh_relation[1]=relation[1]+1;
        }
        result.push_back( cnh_relation );
    }
    return result;
}



// =================================================================================
// =================================================================================
// =================================================================================
// =================================================================================
// =================================================================================
//  PRIVATE METHODS
// =================================================================================
void GPCC::Init () {
    BB_DBG(5) << ">>>>>>> GPCC (Init) ..." << endl;

    m_GPCC_Result.clear();

    m_fOriginX     = 0.0;
    m_fOriginY     = 0.0;
    m_bOriginSet   = false;
    m_ptOrigin.SetXY(0,0);

    m_fRelatumX    = 0.0;
    m_fRelatumY    = 0.0;
    m_bRelatumSet  = false;
    m_ptRelatum.SetXY(0,0);

    m_fRadius      = 0.0;
    m_fTheta       = 0.0;
    m_bThetaSet    = false;

    BB_DBG(5) << "<<<<<<<< GPCC (Init):  ..." << endl;
}

// =================================================================================
// GET Angle and Distance
// =================================================================================
int GPCC::calculateRadii( Point ptReferent ) {
    if( !m_bOriginSet || !m_bRelatumSet) { // insufficient data 4 calc
        return false;
    }
    m_fRadius     = m_ptOrigin.GetDistanceTo( m_ptRelatum );
    BB_DBG(2) << "Radius (Origin<->Relatum : " << m_fRadius
    << "( " << m_ptOrigin << " | " << m_ptRelatum << " )"
    << endl;
    m_fDistRelRef = ptReferent.GetDistanceTo( m_ptRelatum );
    BB_DBG(2) << "Radius (Referent<->Relatum : " << m_fDistRelRef
    << "( " << ptReferent << " | " << m_ptRelatum << " )"
    << endl;
    return true;
}

/* Calculate angle between
   line( referent, origin ) and line( relatum, referent )
 */
int GPCC::calculateTheta( Point ptReferent ) {
    return calculateTheta2( ptReferent );
}
// Calculate Angle(A,B,C) by atan2
int GPCC::calculateTheta1( Point ptReferent ) {
    if( !m_bOriginSet || !m_bRelatumSet) { // insufficient data 4 calc
        return false;
    }

    /*
      if( (m_ptRelatum.X()==m_ptOrigin.X()) ||
    	(ptReferent.X()-m_ptRelatum.X())){
        }
        else{
    */
    m_fTheta = atan2( (ptReferent.Y()-m_ptRelatum.Y())/(ptReferent.X()-m_ptRelatum.X()) ,
                      (m_ptRelatum.Y()-m_ptOrigin.Y())/(m_ptRelatum.X()-m_ptOrigin.X()) );
    //    }
    m_aTheta.set( m_fTheta );
    BB_DBG(7) << "Angle between [Origin, Relatum, Referent] = "
    << m_ptOrigin  << ", "
    << m_ptRelatum << ", "
    << ptReferent  << "] = "
    << m_fTheta << " ( " << m_aTheta << ")" << endl;

    return true;
}
int GPCC::calculateTheta2( Point ptReferent ) {
    if( !m_bOriginSet || !m_bRelatumSet) { // insufficient data 4 calc
        return false;
    }
    Point ptOrigin( m_ptOrigin );
    Point ptRelatum( m_ptRelatum );
    float correctionAngle = 0.0;
    float fErrorEpsilon = 0.0001;
    Point ptTmp(0,0);

    // shift all 3 pt's by (-relatum) => relatum =[0,0]
    ptOrigin.Shift( ptTmp-m_ptRelatum );
    ptRelatum.Shift( ptTmp-m_ptRelatum );
    ptReferent.Shift( ptTmp-m_ptRelatum );
    BB_DBG(5) << "SHIFTED Points: "
    << ptOrigin << ptRelatum << ptReferent
    << endl;

    // rotate ori & ref, such that ori=[-m_fRadius,0]
    // ptTmp.SetXY( -m_fRadius, 0 );
    // correctionAngle = ptOrigin.GetAngleTo( ptTmp );
    correctionAngle = -ptOrigin.GetAngleTo( ptRelatum );

    ptOrigin.Rotate( correctionAngle );
    ptReferent.Rotate( correctionAngle );
    BB_DBG(5) << "ROTATED Points by " << correctionAngle << " rad: "
    << ptOrigin << ptRelatum << ptReferent
    << endl;
    if( (ptOrigin.Y()>fErrorEpsilon) || (ptOrigin.Y()<-fErrorEpsilon) ) {
        cout << "Y Coordinate of Origin is not correct!!!!!!!" << endl;
        exit(1);
    }
    if( ptOrigin.X() >= 0 ) {
        cout << "Origin is on wrong side of coordinate system!!!!!" << endl;
        exit(1);
    }

    // Get the actual angle between the three points
    m_fTheta = ptRelatum.GetAngleTo( ptReferent );
    // ptTmp.SetXY( m_fDistRelRef, 0);
    // m_fTheta = ptReferent.GetAngleTo( ptTmp );
    m_aTheta.set( m_fTheta );
    BB_DBG(5) << "Angle between [Origin, Relatum, Referent] = "
    << m_ptOrigin  << ", "
    << m_ptRelatum << ", "
    << ptReferent  << "] = "
    << m_fTheta << " ( " << m_aTheta << ")" << endl;

    return true;
}

// =================================================================================
// GET Distance Segment
// =================================================================================
std::vector< int > GPCC::calculateDistanceSegment() {
    vector< int > vResult;
    vResult.clear();
    // calculate proportion between m_fRadius and Rel/Ref distance
    float fDistProportion = m_fDistRelRef / m_fRadius;
    BB_DBG(2) << "Distance Proportion [1]: " << fDistProportion << endl;
    float lb, hb;

    // Case for i = 1
    int iSegNo = 1;
    lb = 0.0;
    hb = 1.0 / (float)m_iM;
    if( fDistProportion <= hb ) {
        vResult.push_back( iSegNo );
        if( fDistProportion == hb ) {
            vResult.push_back( iSegNo+1 );
        }
    }
    // Cases for i <= m
    if( vResult.size() == 0) {
        for( iSegNo=2; iSegNo <= m_iM; iSegNo++) {
            lb = hb;
            hb = (float)iSegNo / (float)m_iM;
            if( (lb <= fDistProportion) && (fDistProportion <= hb) ) {
                vResult.push_back( iSegNo );
                if( fDistProportion == lb ) {
                    vResult.push_back( iSegNo-1 );
                }
                if( fDistProportion == hb ) {
                    vResult.push_back( iSegNo+1 );
                }
            }
        }
    }
    // Cases for m < i < 2m
    if( vResult.size() == 0) {
        for( iSegNo=m_iM; iSegNo < 2*m_iM; iSegNo++) {
            lb = hb;
            hb = (float)m_iM / (2*(float)m_iM-(float)iSegNo);
            if( (lb <= fDistProportion) && (fDistProportion <= hb) ) {
                vResult.push_back( iSegNo );
                if( fDistProportion == lb ) {
                    vResult.push_back( iSegNo-1 );
                }
                if( fDistProportion == hb ) {
                    vResult.push_back( iSegNo+1 );
                }
            }
        }
    }
    // Cases for i <= 2m
    if( vResult.size() == 0) {
        lb = hb;
        if( lb <= fDistProportion ) {
            vResult.push_back( iSegNo );
            if( fDistProportion == lb ) {
                vResult.push_back( iSegNo-1 );
            }
        }
    }

    return vResult;
}

// =================================================================================
// GET Angle Segment
// =================================================================================
std::vector< int > GPCC::calculateOrientationSegment() {
    vector< int > vResult;
    vResult.clear();
    int max = 4*m_iM;
    CAngle lb( 0.0 );
    CAngle hb( 0.0 );
    // From 0 to PI
    for( int iSegNo=1; iSegNo<=max; iSegNo++ ) {
        lb = hb;
        hb.set( ((float)iSegNo*M_PI)/(2.0*(float)m_iM) );
        if( iSegNo==max )
            hb.set( 0.0 );
        BB_DBG(8) << iSegNo << ": "
        << lb << " < "
        << m_aTheta << " < "
        << hb << endl;
        if( (lb <= m_aTheta) && (m_aTheta <= hb) ) {
            vResult.push_back( iSegNo );
            if( lb == m_aTheta) {
                if( iSegNo == 1 ) {
                    vResult.push_back( max );
                } else {
                    vResult.push_back( iSegNo-1 );
                }
            }
            if( m_aTheta == hb ) {
                if( iSegNo == max ) {
                    vResult.push_back( 1 );
                } else {
                    vResult.push_back( iSegNo+1 );
                }
            }
            break;
        }
    }
    return vResult;
}



// =================================================================================
// GET Distance Segment (Segment Border Linear according to OPRA & TPCC)
// only intereesting for the 3 TPCC cases (x=0; 0<x<1; 1<=x
// =================================================================================
std::vector< int > GPCC::calculateDistanceSegmentLinear() {
    vector< int > vResult;
    vResult.clear();
    // calculate proportion between m_fRadius and Rel/Ref distance
    float fDistProportion = m_fDistRelRef / m_fRadius;
    BB_DBG(2) << "Distance Proportion [2]: " << fDistProportion << endl;
    float lb, hb;

    // Case for i = 1
    int iSegNo = 1;
    lb = 0.0;
    hb = 1.0 / (float)m_iM;
    BB_DBG(4) << iSegNo << " : " << fDistProportion << " < " << hb << endl;
    if( fDistProportion < hb ) {
        vResult.push_back( iSegNo );
    }
    BB_DBG(4) << iSegNo+1 << " : " << fDistProportion << " = " << hb << endl;
    if( fDistProportion == hb ) {
        vResult.push_back( iSegNo+1 );
    }

    // Cases for i <= m
    if( vResult.size() == 0) {
        for( iSegNo=2; iSegNo <= m_iM; iSegNo++) {
            lb = hb;
            hb = (float)iSegNo / (float)m_iM;
            BB_DBG(4) << iSegNo+1 << " : " << lb << " < " << fDistProportion << " < " << hb << endl;
            if( (lb < fDistProportion) && (fDistProportion < hb) ) {
                vResult.push_back( iSegNo+1 );
            } else if( fDistProportion == lb ) {
                vResult.push_back( iSegNo );
            } else if( fDistProportion == hb ) {
                vResult.push_back( iSegNo+2 );
            }
        }
    }
    // Cases for m < i < 2m
    if( vResult.size() == 0) {
        for( iSegNo=m_iM; iSegNo < 2*m_iM; iSegNo++) {
            lb = hb;
            hb = (float)m_iM / (2*(float)m_iM-(float)iSegNo);
            BB_DBG(4) << iSegNo+1 << " : " << lb << " < " << fDistProportion << " < " << hb << endl;
            if( (lb < fDistProportion) && (fDistProportion < hb) ) {
                vResult.push_back( iSegNo+1 );
            } else if( fDistProportion == lb ) {
                vResult.push_back( iSegNo );
            } else if( fDistProportion == hb ) {
                vResult.push_back( iSegNo+2 );
            }
        }
    }
    // Cases for i <= 2m
    if( vResult.size() == 0) {
        lb = hb;
        BB_DBG(4) << iSegNo+1 << " : " << lb << " < " << fDistProportion << endl;
        if( lb < fDistProportion ) {
            vResult.push_back( iSegNo );
        } else if( fDistProportion == lb ) {
            vResult.push_back( iSegNo );
        }
    }
    return vResult;
}

// =================================================================================
// GET Angle Segment (Segment Border Linear according to OPRA & TPCC)
// =================================================================================
std::vector< int > GPCC::calculateOrientationSegmentLinear() {
    vector< int > vResult;
    vResult.clear();
    int max = 4*m_iM;
    CAngle lb( 0.0 );
    CAngle hb( 0.0 );
    // From 0 to 2PI

    for( int iSegNo=1; iSegNo<=max; iSegNo+=1 ) {
        lb = hb;
        hb.set( ((float)iSegNo*M_PI)/(2.0*(float)m_iM) );
        if(iSegNo==max)
            hb.set( 0.0 );
        // cout << "lb: " << lb << "   hb: " << hb << endl;
        BB_DBG(2) << 2*(iSegNo-1)+1 << ": "
        << lb << " == "
        << m_aTheta << " ??";

        if( lb == m_aTheta) {
            BB_DBG(2) << "  YES  " << endl;
            vResult.push_back( 2*(iSegNo-1)+1 );
            break;
        } else {
            BB_DBG(2) << "  NO  " << endl;
        }
        BB_DBG(2) << 2*(iSegNo-1)+2 << ": "
        << lb << " < "
        << m_aTheta << " < "
        << hb << " ?? ";
        // if( (lb <= m_aTheta) && (m_aTheta <= hb) ){
        if( (lb < m_aTheta) && (m_aTheta < hb) ) {
            BB_DBG(2) << "  YES  " << endl;
            vResult.push_back( 2*(iSegNo-1)+2 );
            break;
        } else {
            BB_DBG(2) << "  NO  " << endl;
        }

    }
    return vResult;
}
