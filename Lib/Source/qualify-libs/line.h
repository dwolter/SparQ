#ifndef LINE_H
#define LINE_H

#include <stdlib.h>
#include <stdio.h>
#include <cmath>
#include <iostream>

// #include <utils.h>

#include <geometry.h>
#include <point.h>

//#include <draw_widget/draw_widget.h>

template<class T>
T sign( const T &a ) {
    return ( a < (T) 0 ? (T) -1 : a > (T) 0 ? (T) 1 : (T) 0 );
}



class Line {
public:
    Line();
    Line( const float _a,
          const float _b,
          const float _c );

    Line( const Point & p1,
          const Point & p2 );

    Line( const Point & p,
          const float angle );

    Line( const float distance,
          const float angle );

    ~Line() {}

    float a;
    float b;
    float c;

    const float A() const;
    const float B() const;
    const float C() const;
    void SetA( const float _a );
    void SetB( const float _b );
    void SetC( const float _c );

    bool IsDipole;
    Point ptStart;
    Point ptEnd;
    Point GetStartPt( );
    Point GetEndPt( );

    const float GetX( const float y ) const;
    const float GetY( const float x ) const;

    const float GetAngle() const;
    const Line GetNormal( const Point & p ) const;

    /**
     * Calculates the cut with another line
     * 
     * @param line the line to cut with
     * @param cut the cut, if it exists
     * 
     * @return 0, if the cut is a point 
     *         1, if the lines are equal
     *         2, if the lines are parallel and not equal
     *         -1, if something went wrong
     */
    const int GetCut( const Line & line,
                      Point & cut ) const;

    const bool IsParallelTo( const Line & line ) const;
    const bool IsPointOnLine( const Point & p ) const;
    const Point GetNearestPointTo( const Point & p ) const;
    const float GetSquaredDistanceTo( const Point & p ) const;
    const float GetDistanceTo( const Point & p ) const;
    /// returns:
    /// -1 if p is left of the line
    ///  0 if p is on the line
    /// +1 if p is right of the line
    const int GetSide( const Point & p ) const;
    // if Line defined as oriented line segment (dipole)
    bool rightOfLine( Point pt) ;
    bool leftOfLine(  Point pt );
    int GetSide( Point p );
    /// returns:
    /// -1 if p behind start point of line
    ///  0 if p between start and end point (including points)
    /// +1 if p in front of end point of line
    int PositionOnLineSegment( Point pt );

    const bool operator==( const Line & l2 ) const;

    inline friend std::ostream & operator<<( std::ostream & ostr,
            const Line & l ) {
        l.PrintToStream( ostr );
        return ostr;
    }

    void Display( const float x_start,
                  const float x_stop,
                  //		DrawWidget * p_draw_widget,
                  const float draw_scale
                ) const;
    void PrintToStream( std::ostream & ostr ) const;
};

inline Line::Line() {
    a = 1.0;
    b = 0.0;
    c = 0.0;
    IsDipole = false;
}

inline Line::Line( const float _a,
                   const float _b,
                   const float _c ) {
    a = _a;
    b = _b;
    c = _c;
    IsDipole = false;
}

inline Line::Line( const Point & p1,
                   const Point & p2 ) {
    if( p1.Y() == p2.Y()) {
        a = 0.0;
        b = 1.0;
        c = - ( b * p1.Y());
    } else {
        a = 1.0;
        b = ( p1.X() - p2.X()) * a / ( p2.Y() - p1.Y());
        c = - ( p1.X() * a + b * p1.Y());
    }
    IsDipole = true;
    ptStart = p1;
    ptEnd   = p2;
}

inline Line::Line( const Point & p,
                   const float angle ) {
    /*   a = cos( angle ); */
    /*   b = sin( angle ); */
    /*   c = - p.X() * a - p.Y() * b; */
    a = sin( angle );
    b = -cos( angle );
    c = - ( p.X() * a + p.Y() * b);
    IsDipole = false;
}

inline const float Line::A() const {
    return a;
}

inline const float Line::B() const {
    return b;
}

inline const float Line::C() const {
    return c;
}

inline void Line::SetA( const float _a ) {
    a = _a;
}

inline void Line::SetB( const float _b ) {
    b = _b;
}

inline void Line::SetC( const float _c ) {
    c = _c;
}


inline Point Line::GetStartPt( ) {
    if( !IsDipole ) {
        return Point();
    }
    return ptStart;
}

inline Point Line::GetEndPt( ) {
    if( !IsDipole ) {
        return Point();
    }
    return ptEnd;
}


inline const float Line::GetX( const float y ) const {
    return (( - c - b * y ) / a );
}

inline const float Line::GetY( const float x ) const {
    return (( -c - a * x ) / b );
}

inline const float Line::GetAngle() const {
    return atan2f( a, - b );
}

inline const Line Line::GetNormal( const Point & p ) const {
    return Line( -b , a , - ( -b * p.X() + a * p.Y()));
}

inline void Line::PrintToStream( std::ostream & ostr ) const {
    ostr << a << "x + " << b << "y + " << c << " = 0";
}

inline const bool Line::IsParallelTo( const Line & line ) const {
    if( line.A() == 0.0 ) {
    return ( a == 0.0 ) || ( line.B() == 0.0 );
    } else if( line.B() == 0.0 ) {
    return ( b == 0.0 ) || ( line.A() == 0.0 );
    }

    return ( a / line.A()) == ( b / line.B());
}

inline const int Line::GetCut( const Line & line,
                               Point & cut ) const {
                                   /**
                                    * the lines are parallel if the vectors ( a1 b1 ), ( a2 -b2) are
                                    * linearly dependendant, which means that ... ? 
                                    */
                                   // if( a * -line.B() - line.A() * b )
                                   if( IsParallelTo( line )) {
                                   /**
                                    * The lines are parallel
                                    */
                                   return ( c == line.C() ? 1 : 2 );
                                   }

                                   if(( a != 0.0 ) && ( line.A() != 0.0 )) {
                                   float zaehler = ( a * line.C() / line.A()) - c;
                                       float nenner = ( b - ( a * line.B() / line.A()));

                                       cut.SetY( zaehler / nenner );
                                       cut.SetX(( -line.C() - line.B() * cut.Y()) / line.A());

                                       return 0;
                                   } else if(( b != 0.0 ) && ( line.A() != 0.0 )) {
                                   cut.SetY( - c / b );
                                       cut.SetX(( - line.C() + ( line.B() * c ) / b ) / line.A());
                                       return 0;
                                   } else if(( line.B() != 0.0 ) && ( a != 0.0 )) {
                                   cut.SetY( - line.C() / line.B());
                                       cut.SetX(( - c + ( b * line.C()) / line.B()) / a );
                                       return 0;
                                   } else if( b != 0.0 ) {
                                   std::cerr << " * Line: Wie kann a == 0.0 und line.A() == 0.0 sein, die Linien müssten parallel sein!" << std::endl;
                                   return -1;
                                   } else {
                                   std::cerr << " * Line: Wie kann a == 0.0 UND b == 0.0 sein? Das ist keine Linie, sondern eine Ebene!" << std::endl;
                                   return -1;
                                   }

                                   return -1;
                               }

inline const bool Line::IsPointOnLine( const Point & p ) const {
    return ( a * p.X() + b * p.Y() == -c );
}

inline const Point Line::GetNearestPointTo( const Point & p ) const {
    if( IsPointOnLine( p )) {
    return p;
    } else {
    static Point result;

    GetCut( GetNormal( p ),
            result );

        return result;
    }
}

inline const float Line::GetSquaredDistanceTo( const Point & p ) const {
    return p.GetSquaredDistanceTo( GetNearestPointTo( p ));
}

inline const float Line::GetDistanceTo( const Point & p ) const {
    return p.GetDistanceTo( GetNearestPointTo( p ));
}

inline const int Line::GetSide( const Point & p ) const {
    return ((int) sign( a * p.X() + b * p.Y() + c ));
}

inline void Line::Display( const float x_start,
                           const float x_stop,
                           //			   DrawWidget * p_draw_widget,
                           const float draw_scale
                         ) const {
                             /*  p_draw_widget->DrawLine((int) ( draw_scale * x_start ),
                                                       (int) ( draw_scale * GetY( x_start )),
                                                       (int) ( draw_scale * x_stop ),
                                                       (int) ( draw_scale * GetY( x_stop )));*/
                         }


/** true if point is to the right of oriented line segment */
inline bool Line::rightOfLine( Point pt ) {
    if( IsDipole ) {
        return (( ptEnd.x-ptStart.x)*(pt.y-ptStart.y)-(ptEnd.y-ptStart.y)*(pt.x-ptStart.x) < 0);
    }
    return ( GetSide(pt) );
}

/** true if point is to the left of oriented line segment */
//return (l.ex-l.sx)*(y-l.sy)-(l.ey-l.sy)*(x-l.sx) > 0;
inline bool Line::leftOfLine( Point pt) {
    if( IsDipole ) {
        return (( ptEnd.x-ptStart.x)*(pt.y-ptStart.y)-(ptEnd.y-ptStart.y)*(pt.x-ptStart.x) > 0);
    }
    return ( GetSide(pt) );
}

inline int Line::GetSide( Point pt ) {
    if( leftOfLine( pt )) {
        return (-1)
               ;
    }
    if( rightOfLine( pt )) {
        return 1;
    }
    return 0;
}


/// returns:
/// -2 if something wrong
/// -1 if p behind start point of line
///  0 if p between start and end point (including points)
/// +1 if p in front of end point of line
inline int Line::PositionOnLineSegment( Point pt ) {
    float lambdaX, lambdaY;
    if( IsDipole && IsPointOnLine(pt)) {
        lambdaX = (pt.x - ptStart.x) / (ptEnd.x - ptStart.x);
        lambdaY = (pt.y - ptStart.y) / (ptEnd.y - ptStart.y);
        // printf(" Lambda: %f, %f \n", lambdaX, lambdaY);
        if( lambdaX<0 || lambdaY<0)
            return (-1);
        if( lambdaX>1 || lambdaY>1)
            return (1);
        return 0;
    }
    return -2;
}

inline const bool Line::operator==( const Line & l2 ) const {
    return ( (a == l2.A()) && (b == l2.B()) && (c == l2.C()));
}



#endif
