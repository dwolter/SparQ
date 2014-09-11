#ifndef GEOMETRY_H
#define GEOMETRY_H

#include <math.h>

#undef M_PI
#undef M_PI_2
const float M_PI = 3.1415926535897932384626433832795;
const float M_PI_2 = M_PI / 2;
const float SQRT2PI = 2.506628275;
const float DEG_1 = M_PI_2 / 90.0;
const float DEG_20 = M_PI / 9.0;
const float DEG_90 = M_PI_2;
const float DEG_180 = M_PI;
const float DEG_270 = M_PI + M_PI_2;
const float DEG_360 = 2.0 * M_PI;

float halfNormalizedAngleRad(float alpha);
float halfNormalizedAngleDeg(float alpha);
float normalizedAngleRad(float alpha);
float normalizedAngleDeg(float alpha);

bool isInBowRad(float angle,float alpha1, float alpha2);
bool isInBowDeg(float angle,float alpha1, float alpha2);

float getDistPointToLine(float px,
			 float py,
			 float g0x,
			 float g0y,
			 float mgx,
			 float mgy);

float getLengthPointProjection(float px,
			       float py,
			       float g0x,
			       float g0y,
			       float mgx,
			       float mgy);

inline float rad2deg(float rad)
{
    return 180.0 * rad / M_PI; 
}

inline float deg2rad(float deg)
{
    return deg * M_PI / 180.0;
}

inline float fsqr(float x)
{
    return x * x;
}
    
inline double dsqr(double x)
{
    return x * x;
}

/// example: angleDistance(DEG_90,DEG_180) = DEG_90
///          angleDistance(DEG_180,DEG_90) = -DEG_90
float angleDistance(float angle1,
		    float angle2);


inline float angleAverage(float angle1,
			  float angle2)
{
  return normalizedAngleRad(angle1 + 0.5 * angleDistance(angle1,angle2));
}

int getQuadrant( float angle );

void normalizeAngles( float & angle1,
		      float & angle2 );
#endif



