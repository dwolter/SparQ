/* This file is part of SparQ, a toolbox for qualitative spatial reasoning.
   Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Project R3-[Q-Shape]
   More info at http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

  SparQ is free software and has been released under the terms of the GNU
  General Public License version 3 or later. You should have received a
  copy of the GNU General Public License along with this program. If not,
  see <http://www.gnu.org/licenses/>.
*/

#ifndef VECTOR_H
#define VECTOR_H


// Forward declaration
class Vector;

#include <stdlib.h>
#include <math.h>
#include <iostream>
#include <cangle.h>
#include <point.h>

class Vector
{
 public:
  /**
   * Standard Constructor.
   * Will construct (0,0)
   * 
   */
  Vector()
    {
      x = 0.0;
      y = 0.0;
    }
  
    
  /** 
   * constructor
   * 
   * @param x x-coordinate of the vector
   * @param y y-coordinate of the vector
   */
  Vector( const float _x,
	  const float _y )
    {
      x = _x;
      y = _y;
    }

  /** 
   * Copy constructor
   * 
   * @param vector the vector to copy
   */
  Vector( const Vector & v )
    {
      x = v.x;
      y = v.y;
    }

  
  Vector( const Point & p );


  ~Vector() {}

  /**
   * The data is public for easy and fast access.
   */
  float x;
  float y;

  /* 
   * Getter / Setter
   */
  const float X() const;
  const float Y() const;
  void SetX( const float _x );
  void SetY( const float _y );
  void SetXY( const float _x,
	      const float _y );
  
  /**
   * Some special functions
   * 
   */
  void Shift( const Vector & dp );
  void Shift( const float dx,
	      const float dy );
  
  const float GetLength() const;
  void SetLength( float length);
  void Norm();

  const float GetAngle() const;
  const float GetAngleTo( const Vector & p2 ) const;

  void SetAngleLength( float angle, float length);

  /**
   * rotation around (0,0)
   */
  void Rotate( const float angle);

  
  /**
   * Some operators
   * 
   */
  const Vector & operator+=( const Vector & p2 );
  const Vector & operator-=( const Vector & p2 );
  const Vector & operator*=( const float scalar );

  const Vector operator+( const Vector & p2 ) const;
  const Vector operator-( const Vector & p2 ) const;
  const Vector operator*( const float scalar ) const;
  const float operator*( const Vector & v2 ) const;
  
  const Vector & operator=( const Vector & p2 );
  
  const bool operator==( const Vector & p2 ) const;
  float & operator[]( const unsigned int index );
  
  inline friend std::ostream & operator<<( std::ostream & ostr,
					   const Vector & p )
    {
      p.PrintToStream( ostr );
      return ostr;
    }

  void PrintToStream( std::ostream & ostr ) const;
};


/* ----------------------------------------------------------
   implementation
---------------------------------------------------------- */

inline const float Vector::X() const
{
  return x;
}

inline const float Vector::Y() const
{
  return y;
}

inline void Vector::SetX( const float _x ) 
{
  x = _x;
}

inline void Vector::SetY( const float _y )
{
  y = _y;
}

inline void Vector::SetXY( const float _x,
			   const float _y )
{
  x = _x;
  y = _y;
}


inline void Vector::Shift( const Vector & dp )
{
  x += dp.x;
  y += dp.y;
}

inline void Vector::Shift( const float dx,
			  const float dy )
{
  x += dx;
  y += dy;
}

inline const float Vector::GetLength() const
{
  return sqrt( x*x + y*y );
}

inline void Vector::SetLength( float length )
{
  float l = GetLength();
  x *= length / l;
  y *= length / l;
}

inline void Vector::Norm()
{
  SetLength(1.0);
}

inline const float Vector::GetAngle( ) const
{
  return atan2f( y, x );
}


inline const float Vector::GetAngleTo( const Vector & p2 ) const
{
  return CAngle( p2.GetAngle() ).diff( CAngle( GetAngle() ) );
}

inline void Vector::Rotate( const float angle )
{
  float l = GetLength();
  float a = GetAngle();
  x = cos( a + angle ) * l;
  y = sin( a + angle ) * l;
}

inline void Vector::SetAngleLength( float angle, float length)
{
  x = cos( angle ) * length;
  y = sin( angle ) * length;
}


/* ----------------------------------------------------------
   operators
---------------------------------------------------------- */

inline const Vector & Vector::operator+=( const Vector & p2 )
{
  x += p2.x;
  y += p2.y;
  
  return (*this);
}

inline const Vector & Vector::operator-=( const Vector & p2 )
{
  x -= p2.x;
  y -= p2.y;
  
  return (*this);
}

inline const Vector & Vector::operator*=( const float scalar )
{
  x *= scalar;
  y *= scalar;
  
  return (*this);
}

inline const Vector Vector::operator+( const Vector & p2 ) const
{
  Vector p( x + p2.x,
	    y + p2.y);
  
  return p;
}

inline const Vector Vector::operator-( const Vector & p2 ) const 
{
  Vector p( x - p2.x,
	    y - p2.y);
  
  return p;
}

inline const Vector Vector::operator*( const float scalar ) const 
{
  Vector p( x * scalar,
	    y * scalar );
  
  return p;
}

inline const float Vector::operator*( const Vector & v2 ) const
{
  return ( x * v2.x + y * v2.y );
}

inline const Vector & Vector::operator=( const Vector & p2 )
{
  x = p2.x;
  y = p2.y;
  
  return (*this);
}

inline const bool Vector::operator==( const Vector & p2 ) const
{
  return ( x == p2.x) && ( y == p2.y);
}

inline void Vector::PrintToStream( std::ostream & ostr ) const
{
  ostr << "[" << x << "," << y << "]";
}

#endif
