#ifndef C_ANGLE_H
#define C_ANGLE_H

#include <iostream>
#include <geometry.h>
#include <math.h>


/** class for angles, always normalized to -PI..PI 
 * @author: fritz@cs.rwth-aachen.de
*/
class CAngle
{
 public:
  CAngle();
  CAngle(const CAngle & a) {
    m_angle_rad = a.get();
  }
  CAngle(const float rad);
  ~CAngle();
  
  inline float get() const {
    return m_angle_rad;
  }
  
  void set(float rad);

  void rotate(float rad) {
    m_angle_rad += rad;
    norm();
  }

  float diff(float rad);

  float diff(const CAngle &p2) {
    return diff(p2.get());
  }

  float diff_right(float rad) {
    return norm_2pi( rad - m_angle_rad );
  }

  float diff_right(const CAngle &p2) {
    return diff_right(p2.get());
  }

  CAngle operator+(const CAngle & p2) const;
  CAngle operator-(const CAngle & p2) const;
  CAngle operator*(const float scalar) const;

  CAngle & operator+=(const CAngle & p2);
  CAngle & operator-=(const CAngle & p2);
  CAngle & operator*=(const float scalar);  
  CAngle & operator=(const CAngle & p2);

  bool operator<(const CAngle & p2);
  bool operator>(const CAngle & p2);
  bool operator>=(const CAngle & p2);
  bool operator<=(const CAngle & p2);
  bool operator==(const CAngle & p2);


  void PrintToStream (std::ostream &ostr) const
    {
      ostr << "(" << m_angle_rad << ")";
    }
  friend std::ostream &operator<<(std::ostream &ostr, const CAngle &p) {
    p.PrintToStream(ostr); return ostr; 
  }

 private:

  /** normalize to -PI..PI */
  float norm(float rad);
  void norm();
  float norm_2pi(float rad);

  float m_angle_rad;


};

#endif
