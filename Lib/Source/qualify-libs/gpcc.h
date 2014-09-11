/* This file is part of SparQ, a toolbox for qualitative spatial reasoning.
   Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
   More info at http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

  SparQ is free software and has been released under the terms of the GNU
  General Public License version 3 or later. You should have received a
  copy of the GNU General Public License along with this program. If not,
  see <http://www.gnu.org/licenses/>.
*/

#ifndef BB_DEBUG
#define BB_DEBUG 0
#endif
#define BB_DBG(level) \
if(BB_DEBUG>=level ) \
    ::std::cout << "(" << ((int)(level)) << ") "

    #ifndef _GPCC_H_
    #define _GPCC_H_

    /* Class for getting qualitative results based on the GPCC
     */

    #include <vector>
    #include <point.h>
    #include <cangle.h>


    using namespace std;

class GPCC {
public:
    // constructor
    GPCC( int m, float relatumX, float relatumY, float originX, float originY);
    GPCC(int m);
    GPCC();
    void GPCCConstruct( int m,
                              float relatumX, float relatumY,
                              float originX , float originY );

    // standard destructor
    virtual ~GPCC() {}

    // methods
    int                getScale();
    int                setScale(int m);

    int                setOrigin( float x, float y);
    int                setOrigin( Point ptOrigin);
    int                getOrigin( float &x, float &y);
    Point              getOrigin( );
    int                setRelatum( float x, float y);
    int                setRelatum( Point ptRelatum );
    int                getRelatum( float &x, float &y);
    Point              getRelatum( );


    // get single segment number for referent point (x,y) resp.
    // of the point (dist, ori) relative to the relatum
    /* [0,-2] : Origin != Relatum  =  Referent (sam)
       [0,-1] : Origin =  Relatum  != Referent (dou)
       [0,0]  : Origin =  Relatum  =  Referent (tri)
       [p,q]  : p>0 is distance segment and 0<q<=8m is orientation segment
    (count mathematically positive from inside to outside)
     */
    std::vector< int > getSectorNo( float x, float y );
    std::vector< int > getSectorNo( float x, float y, float error);

    // List of all possible sector Tupels
    std::vector< std::vector< int > > getSectorTupel( Point referent );
    std::vector< std::vector< int > > getSectorTupel( float x, float y );
    std::vector< std::vector< int > > getSectorTupel( float x, float y, float error);

    int                getMaxSectors();
    int                getMaxSectors( int m);

    std::vector< std::vector< int > > getNeighborhoodRelations( std::vector< int > relation );

    /* These methods are for naming the relations differently
       In the original GPCC linear relations are described by
       disjunctions of the neighboring planar relations.
       By setting bLinear=true linear relations have their own
       numbers according to TPCC or OPRA
     */
    bool               isLinear();
    void               setLinear( bool bLinear );

private:
    /* ************************************************************************ */
    /* PRIVATE METHODS                                                          */
    /* ************************************************************************ */
    void Init(); // init stuff 2be done in all constructors
    int calculateRadii( Point ptReferent );
    int calculateTheta( Point ptReferent);
    // different Methods for calculating theta
    int calculateTheta1( Point ptReferent);
    int calculateTheta2( Point ptReferent);
    int calculateTheta3( Point ptReferent);

    std::vector< int > calculateOrientationSegment();
    std::vector< int > calculateDistanceSegment();
    std::vector< int > calculateOrientationSegmentLinear();
    std::vector< int > calculateDistanceSegmentLinear();

    /* ************************************************************************ */
    /* PRIVATE VARIABLES                                                        */
    /* ************************************************************************ */
    std::vector< int >               m_GPCC_Result;

    int                              m_iM;

    float                            m_fOriginX;
    float                            m_fOriginY;
    bool                             m_bOriginSet;
    Point                            m_ptOrigin;

    float                            m_fRelatumX;
    float                            m_fRelatumY;
    bool                             m_bRelatumSet;
    Point                            m_ptRelatum;
    //    float                            m_fRelatumOrientation; // (== m_fAngleOriRel)

    //    float                            m_fAngleOriRel;
    //    float                            m_fDistOriRel;

    float                            m_fRadius;
    float                            m_fDistRelRef;
    float                            m_fTheta;
    CAngle                           m_aTheta;
    bool                             m_bThetaSet; // possible to set directly 4 OPRA

    bool                             m_bIsLinear;

    /*    std::vector< std::vector< int > > vTupelResult;
        std::vector< int > v1;
        std::vector< int > v2;
        std::vector< int > v3;
        std::vector< int > v4;*/

};


#endif
