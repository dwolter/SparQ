#include <cangle.h>


float max( float x1, float x2 )
{
  if( x1 > x2 ) return x1;
  return x2;
}
float min( float x1, float x2 )
{
  if( x1 < x2 ) return x1;
  return x2;
}

CAngle::CAngle()
{
   m_angle_rad = 0.0; 
}

CAngle::CAngle(const float rad)
{
  m_angle_rad = norm(rad);
}

CAngle::~CAngle()
{
}

// normalize
float CAngle::norm(float rad) {
  while(rad > M_PI) rad -= 2*M_PI;
  while(rad <= -M_PI) rad += 2*M_PI;
  return rad;
}

void CAngle::norm() {
  m_angle_rad = norm(m_angle_rad);
}


float CAngle::norm_2pi(float rad) {
  while(rad >= 2*M_PI) rad -= 2*M_PI;
  while(rad < 0) rad += 2*M_PI;
  return rad;
}

void CAngle::set(const float rad)
{
  m_angle_rad = norm(rad);
}

float CAngle::diff(float rad) {
  return norm(m_angle_rad - rad);
}


// operators 

CAngle CAngle::operator+(const CAngle & p2) const
{
  return CAngle( m_angle_rad + p2.get());
}

CAngle CAngle::operator-(const CAngle & p2) const
{
  return CAngle( m_angle_rad - p2.get());
}

CAngle CAngle::operator*(const float scalar) const
{
  return CAngle(m_angle_rad * scalar);
}

CAngle & CAngle::operator+=(const CAngle & p2)
{
  *this = *this + p2;
   return *this;
}

CAngle & CAngle::operator-=(const CAngle & p2)
{
   *this = *this - p2;
   return *this;
}

CAngle & CAngle::operator*=(const float scalar)
{
   *this = *this * scalar;
   return *this;
}

CAngle & CAngle::operator=(const CAngle & p2)
{
  m_angle_rad = p2.get();
  return *this;
}


// comparison operators

bool CAngle::operator<(const CAngle & p2){
  return (diff(p2.get()) < 0.0);
}
bool CAngle::operator>(const CAngle & p2){
  return (diff(p2.get()) > 0.0);
}
bool CAngle::operator<=(const CAngle & p2){
  return (diff(p2.get()) <= 0.0);
}
bool CAngle::operator>=(const CAngle & p2){
  return (diff(p2.get()) >= 0.0);
}

bool CAngle::operator==(const CAngle & p2){
  bool result;
  double epsilon = 0.0001;

  // double diff1 = fabs( norm(m_angle_rad-p2.get()));
  // double diff2 = norm(min(m_angle_rad,p2.get())+ norm(2*M_PI-max(m_angle_rad,p2.get())));
  // double diff = min( fabs( norm(m_angle_rad-p2.get())), norm(min(m_angle_rad,p2.get())+ norm(2*M_PI-max(m_angle_rad,p2.get()))));
  // printf( "CAngle: %f or %f < %f ??\n", diff1, diff2, epsilon);
  // if( diff<=epsilon ) return true;
  //return false;
  

  if( m_angle_rad == p2.get() ){
    // printf( "CAngle TRUE: %f == %f \n", m_angle_rad, p2.get());
    return true;
  }
  if( ((m_angle_rad-epsilon) < p2.get()) && ((m_angle_rad+epsilon) > p2.get())){
    // printf( "CAngle TRUE: %f < %f < %f\n", (m_angle_rad-epsilon),p2.get(), (m_angle_rad+epsilon));
    return true;
  }
  // return (m_angle_rad == p2.get());
  // printf( "CAngle FALSE: %f < %f < %f\n", (m_angle_rad-epsilon),p2.get(), (m_angle_rad+epsilon));
  return false;
}

// boolean CAngle::operator>=(const CAngle & p2) {
//   return true;
// }
// boolean CAngle::operator=<(const CAngle & p2) {
//   return true;
// }
// boolean CAngle::operator==(const CAngle & p2) {
//   return (m_angle_rad = p2.get());
// }


