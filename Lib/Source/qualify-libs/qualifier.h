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


    #ifndef _QUALIFIER_H_
    #define _QUALIFIER_H_

    #include <vector>
    #include <string>
    #include <point.h>
    #include <cangle.h>
    #include <vector.h>
    #include <line.h>


    /*****************************************************************************
    *** Comments:
    ***     - all angles in rad
    ***     - Vectors resp. dx/dy are the local shift
    *****************************************************************************/

    /*****************************************************************************
    *** OPRA
    *****************************************************************************/
    int		getOPRA_MaxSectors( int m);


// returns 1 element  in vector if pos(A) == pos(B)
// returns 2 elements in vector if pos(A) != pos(B)
// input in polar coordinates

std::string getOPRA_Relation_String(int m,
                                    Point ptA, double oriA,
                                    Point ptB, double oriB);
std::string getOPRA_Relation_String(int m,
                                    const double Ax, const double Ay, double Aori,
                                    const double Bx, const double By, double Bori);

std::vector< int > getOPRA_Relation(int m,
                                    Point ptA, double oriA,
                                    Point ptB, double oriB);
std::vector< int > getOPRA_Relation(int m,
                                    const double Ax, const double Ay, double Aori,
                                    const double Bx, const double By, double Bori);

// input in cartesian coordinates
std::string getOPRA_Relation_String(int m,
                                    Point ptA, Vector vOriA,
                                    Point ptB, Vector vOriB);

std::string getOPRA_Relation_String(int m,
                                    const double Ax, const double Ay, double Adx, double Ady,
                                    const double Bx, const double By, double Bdx, double Bdy);

std::vector< int > getOPRA_Relation(int m,
                                    Point ptA, Vector vOriA,
                                    Point ptB, Vector vOriB);

std::vector< int > getOPRA_Relation(int m,
                                    const double Ax, const double Ay, double Adx, double Ady,
                                    const double Bx, const double By, double Bdx, double Bdy);


/*****************************************************************************
*** GPCC
*****************************************************************************/
int		getGPCC_MaxSectors( int m );
int		getGPCC_MaxRelations( int m );

std::vector< std::vector< int > > getGPCC_All_Base_Relations( int m );
std::string getGPCC_All_Base_Relations_String( int m );

std::string getGPCC_Relation_String(int m,
                                    Point ptOrigin,
                                    Point ptRelatum,
                                    Point ptReferent);

std::string getGPCC_Relation_String(int m,
                                    const double originX, const double originY,
                                    const double relatumX, const double relatumY,
                                    const double referentX, const double referentY);


// returns list of [distance_segment, orientation_segment]
std::vector< std::vector< int > > getGPCC_Relation(int m,
        Point ptOrigin,
        Point ptRelatum,
        Point ptReferent);
std::vector< std::vector< int > > getGPCC_Relation(int m,
        const double originX, const double originY,
        const double relatumX, const double relatumY,
        const double referentX, const double referentY);

std::vector< std::vector< int > > getGPCC_CNH_Relations( int m, std::vector< int > relation );
std::string getGPCC_CNH_Relations_String( int m, std::vector< int > relation );
std::string getGPCC_CNH_Relations_String( int m, std::string relation );

/*****************************************************************************
*** GTPCC
*****************************************************************************/
int		getGTPCC_MaxSectors( int m );
int		getGTPCC_MaxRelations( int m );

std::vector< std::vector< int > > getGTPCC_All_Base_Relations( int m );
std::string getGTPCC_All_Base_Relations_String( int m );

std::string getGTPCC_Relation_String(int m,
                                    Point ptOrigin,
                                    Point ptRelatum,
                                    Point ptReferent);

std::string getGTPCC_Relation_String(int m,
                                    const double originX, const double originY,
                                    const double relatumX, const double relatumY,
                                    const double referentX, const double referentY);


// returns list of [distance_segment, orientation_segment]
std::vector< std::vector< int > > getGTPCC_Relation(int m,
        Point ptOrigin,
        Point ptRelatum,
        Point ptReferent);
std::vector< std::vector< int > > getGTPCC_Relation(int m,
        const double originX, const double originY,
        const double relatumX, const double relatumY,
        const double referentX, const double referentY);

std::vector< std::vector< int > > getGTPCC_CNH_Relations( int m, std::vector< int > relation );
std::string getGTPCC_CNH_Relations_String( int m, std::vector< int > relation );
std::string getGTPCC_CNH_Relations_String( int m, std::string relation );

/*****************************************************************************
*** Dipole Calculus (DC) 
*****************************************************************************/
// m = 	0	DRA  (Schlieder's origin)
// m = 	1	DRA_c  (Schlieder extended with 3 points on a line are allowed)
//	2	DRA_f  (fine grained variant with 72 basic relations)
//	3	DRA_fp (fine grained variant with parallelity => 80 basic relations)

#define DRA 	0
#define DRA_c 	1
#define DRA_f 	2
#define DRA_fp 	3
int		getDC_MaxRelations( int m );

// returns list of 4 or 5 char (depending on m)
//	with char 0-3: l/r/e/s/b/i/f
//	with char 4  : +/-/P/A
std::vector< char > getDC_Relation( int m,
                                    Point ptStartA,
                                    Point ptEndA,
                                    Point ptStartB,
                                    Point ptEndB);

std::vector< char > getDC_Relation(int m,
                                   float ptStartAx, float ptStartAy,
                                   float ptEndAx, float ptEndAy,
                                   float ptStartBx, float ptStartBy,
                                   float ptEndBx, float ptEndBy);

std::string getDC_Relation_String( int m,
                                   Point ptStartA,
                                   Point ptEndA,
                                   Point ptStartB,
                                   Point ptEndB);

std::string getDC_Relation_String(int m,
                                  float ptStartAx, float ptStartAy,
                                  float ptEndAx, float ptEndAy,
                                  float ptStartBx, float ptStartBy,
                                  float ptEndBx, float ptEndBy);

/*****************************************************************************
*** ProjectionBasedAbsoluteDirectionCalculus (PBADC)
*****************************************************************************/
int getPBADC_Relation( int m, Point ptOrigin, Point ptRelatum, float angleRelative=0);
int getPBADC_Relation( int m, double ptOriginX, double ptOriginY, double ptRelatumX, double ptRelatumY, float angleRelative=0);
int getPBADC_Relation( Point ptOrigin, Point ptRelatum, float angleRelative=0);
int getPBADC_Relation( double ptOriginX, double ptOriginY, double ptRelatumX, double ptRelatumY, float angleRelative=0);

std::string getPBADC_Relation_String( int m, Point ptOrigin, Point ptRelatum, float angleRelative=0);
std::string getPBADC_Relation_String( int m, double ptOriginX, double ptOriginY, double ptRelatumX, double ptRelatumY, float angleRelative=0);
std::string getPBADC_Relation_String( Point ptOrigin, Point ptRelatum, float angleRelative=0);
std::string getPBADC_Relation_String( double ptOriginX, double ptOriginY, double ptRelatumX, double ptRelatumY, float angleRelative=0);

/*****************************************************************************
*** Single-Cross Calculus (SCC) 
*****************************************************************************/
std::string getSCC_Relation_String( int m,
                                    Point ptOrigin, Point ptRelatum, Point ptReferent);
std::string getSCC_Relation_String( int m,
                                    double ptOriginX, double ptOriginY,
                                    double ptRelatumX, double ptRelatumY,
                                    double ptReferentX, double ptReferentY);

// the basic version of the SCC
std::string getSCC_Relation_String( Point ptOrigin, Point ptRelatum, Point ptReferent);
std::string getSCC_Relation_String( double ptOriginX, double ptOriginY,
                                    double ptRelatumX, double ptRelatumY,
                                    double ptReferentX, double ptReferentY);
const char* qualify_scc( double ptOriginX, double ptOriginY,
			 double ptRelatumX, double ptRelatumY,
			 double ptReferentX, double ptReferentY);


/*****************************************************************************
*** Double-Cross Calculus (DCC) 
*****************************************************************************/
// Scalable DoubleCross according to the original paper from 1992
// except the orientation of the segment enumeration is mathematically positive
std::string getDCC_Relation_String( int m,
                                    Point ptOrigin, Point ptRelatum, Point ptReferent);
std::string getDCC_Relation_String( int m,
                                    double ptOriginX, double ptOriginY,
                                    double ptRelatumX, double ptRelatumY,
                                    double ptReferentX, double ptReferentY);

// transfers relations of type 'x_y' into single (number) notation (0-12, A, B, dou, tri)
std::string transferDCCtoDCC2( std::string relation );

// the basic version of the DCC
std::string getDCC_Relation_String( Point ptOrigin, Point ptRelatum, Point ptReferent);
std::string getDCC_Relation_String( double ptOriginX, double ptOriginY,
                                    double ptRelatumX, double ptRelatumY,
                                    double ptReferentX, double ptReferentY);

// transfers relations of type 'x_y' into single (number) notation (0-12, A, B, dou, tri)
std::string transferDCCtoDCC2( std::string relation );

// the basic version of the DCC (with single notation)
std::string getDCC2_Relation_String( Point ptOrigin, Point ptRelatum, Point ptReferent);
std::string getDCC2_Relation_String( double ptOriginX, double ptOriginY,
                                    double ptRelatumX, double ptRelatumY,
                                    double ptReferentX, double ptReferentY);


/*****************************************************************************
*** Ternary Point Configuration Calculus (TPCC) 
*****************************************************************************/
std::string getTPCC_Relation_String( Point ptOrigin, Point ptRelatum, Point ptReferent);
std::string getTPCC_Relation_String( double ptOriginX, double ptOriginY,
                                     double ptRelatumX, double ptRelatumY,
                                     double ptReferentX, double ptReferentY);

std::string getTPCC_All_Base_Relations_String( );


const char* qualify_tpcc( double ptOriginX, double ptOriginY,
			 double ptRelatumX, double ptRelatumY,
			 double ptReferentX, double ptReferentY);


/*****************************************************************************
*** Qualitative Trajectory Calculus (QTC) - by vdWeghe
*****************************************************************************/
std::string getQTC_Relation_String( double pt_K1_X, double pt_K1_Y,
                                    double pt_K2_X, double pt_K2_Y,
                                    double pt_L1_X, double pt_L1_Y,
                                    double pt_L2_X, double pt_L2_Y);

std::string getQTC_Relation_String( Point pt_K1, Point pt_K2, Point pt_L1, Point pt_L2);

std::string getQTC_All_Base_Relations_String( );


/*****************************************************************************
*** TOOLS for distance and orientation segment calculation
*****************************************************************************/
int calculateOrientationSegmentOPRA( int m, double angle );

/*****************************************************************************
*** TOOL for converting G(T)PCC relation structure to string
*****************************************************************************/
std::string relations2String_GPCC_GTPCC( std::vector< std::vector < int > > vRelations);

#endif
