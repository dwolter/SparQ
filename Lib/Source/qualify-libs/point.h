#ifndef POINT_H
#define POINT_H

// Forward declaration
class Point;

#include <stdlib.h>
#include <math.h>
#include <iostream>

#include <vector.h>

class Point
{
public:
  /**
   * Standard Constructor.
   * Will construct (0,0)
   * 
   */
  Point()
  {
    x = 0.0;
    y = 0.0;
  }

  /**
   * constructor
   * 
   * @param x x-coordinate of the point
   * @param y y-coordinate of the point
   */
  Point( const float _x,
         const float _y )
  {
    x = _x;
    y = _y;
  }

  /**
   * Copy constructor
   * 
   * @param point the point to copy
   */
  Point( const Point & p )
  {
    x = p.X();
    y = p.Y();
  }


  ~Point() {}

  /**
   * The data is public for easy and fast access.
   */
  float x;
  float y;

  /**
   * Access functions
   * 
   */
  const float X() const;
  const float Y() const;

  void SetX( const float _x );
  void SetY( const float _y );
  void SetXY( const float _x, const float _y);

  /**
   * Some special functions
   * 
   */
  void Shift( const Point & dp );
  void Shift( const float dx,
              const float dy );

  const Point GetShift( const Point & dp );
  const Point GetShift( const float dx,
                        const float dy );

  const float GetSquaredDistanceTo( const Point & p2 ) const;
  const float GetDistanceTo( const Point & p2 ) const;

  const float GetAngleTo( const Point & p2 ) const;

  /**
   * rotation around (0,0)
   */
  void Rotate( const float angle);

  /**
   * rotation around center
   */
  void Rotate( const float angle,
               const Point & center );

  const Point GetRotate( const float angle ) const;
  const Point GetRotate( const float angle,
                         const Point & center ) const;

  /**
   * Some operators
   * 
   */
  const Point & operator+=( const Point & p2 );
  const Point & operator+=( const Vector & v );
  const Point & operator-=( const Point & p2 );
  const Point & operator-=( const Vector & v );
  const Point & operator*=( const float scalar );

  const Point operator+( const Point & p2 ) const;
  const Point operator+( const Vector & v ) const;
  const Point operator-( const Point & p2 ) const;
  const Point operator-( const Vector & v ) const;
  const Point operator*( const float scalar ) const;

  const Point & operator=( const Point & p2 );

  const bool operator==( const Point & p2 ) const;
  float & operator[]( const unsigned int index );

  inline friend std::ostream & operator<<( std::ostream & ostr,
      const Point & p )
  {
    p.PrintToStream( ostr );
    return ostr;
  }

  void PrintToStream( std::ostream & ostr ) const;
};

inline const float Point::X() const
{
  return x;
}

inline const float Point::Y() const
{
  return y;
}

inline void Point::SetX( const float _x )
{
  x = _x;
}

inline void Point::SetY( const float _y )
{
  y = _y;
}

inline void Point::SetXY( const float _x, const float _y)
{
  x = _x;
  y = _y;
}

inline void Point::Shift( const Point & dp )
{
  x += dp.X();
  y += dp.Y();
}

inline void Point::Shift( const float dx,
                          const float dy )
{
  x += dx;
  y += dy;
}

inline const Point Point::GetShift( const Point & dp )
{
  Point p( X(), Y());

  p.Shift( dp );

  return p;
}

inline const Point Point::GetShift( const float x,
                                    const float y )
{
  Point p( X(), Y());

  p.Shift( x, y );

  return p;
}

inline const float Point::GetSquaredDistanceTo( const Point & p2 ) const
{
  return ( p2.X() - X()) * ( p2.X() - X()) + ( p2.Y() - Y()) * ( p2.Y() - Y());
}

inline const float Point::GetDistanceTo( const Point & p2 ) const
{
  return sqrt( GetSquaredDistanceTo( p2 ));
}

inline const float Point::GetAngleTo( const Point & p2 ) const
{
  // printf("atan2f: %f\n", atan2f( p2.Y() - y, p2.X() - x ));
  return atan2f( p2.Y() - y,
  p2.X() - x );
}

  inline void Point::Rotate( const float angle )
{
  static float sinus;
  static float cosinus;
  static float x_tmp;

  sinus = sin( angle );
  cosinus = cos( angle );

  x_tmp = x * cosinus - y * sinus;
  y = x * sinus + y * cosinus;
  x = x_tmp;
}

  inline void Point::Rotate( const float angle,
  const Point & center )
{
  Shift( -center.X(),
  -center.Y() );

  Rotate( angle );

  Shift( center.X(),
  center.Y() );
}

  inline const Point Point::GetRotate( const float angle ) const
{
  Point p( X(), Y());

  p.Rotate( angle );

  return p;
}

  inline const Point Point::GetRotate( const float angle,
  const Point & center ) const
{
  Point p( x - center.X(),
  y - center.Y());

  p.Rotate( angle );

  p.Shift( center.X(),
  center.Y());

  return p;
}

  inline const Point & Point::operator+=( const Point & p2 )
{
  x += p2.X();
  y += p2.Y();

  return (*this);
}

  inline const Point & Point::operator+=( const Vector & v )
{
  x += v.X();
  y += v.Y();

  return (*this);
}

  inline const Point & Point::operator-=( const Point & p2 )
{
  x -= p2.X();
  y -= p2.Y();

  return (*this);
}

  inline const Point & Point::operator-=( const Vector & v )
{
  x -= v.X();
  y -= v.Y();

  return (*this);
}

  inline const Point & Point::operator*=( const float scalar )
{
  x *= scalar;
  y *= scalar;

  return (*this);
}

  inline const Point Point::operator+( const Point & p2 ) const
{
  Point p( x + p2.X(),
  y + p2.Y());

  return p;
}

  inline const Point Point::operator+( const Vector & v ) const
{
  Point p( x + v.X(),
  y + v.Y());

  return p;
}

  inline const Point Point::operator-( const Point & p2 ) const
{
  Point p( x - p2.X(),
  y - p2.Y());

  return p;
}

  inline const Point Point::operator-( const Vector & v ) const
{
  Point p( x - v.X(),
  y - v.Y());

  return p;
}

  inline const Point Point::operator*( const float scalar ) const
{
  Point p( x * scalar,
  y * scalar );

  return p;
}

  inline const Point & Point::operator=( const Point & p2 )
{
  x = p2.X();
  y = p2.Y();

  return (*this);
}

  inline const bool Point::operator==( const Point & p2 ) const
{
  return ( x == p2.X()) && ( y == p2.Y());
}

  inline void Point::PrintToStream( std::ostream & ostr ) const
{
  ostr << "(" << x << "," << y << ")";
}


  /**
  * Construct a vector from a point
  *
  * @param point The point to be regarded as a vector
  */
  inline Vector::Vector( const Point & p )
{
  x = p.x;
  y = p.y;
}

#endif
