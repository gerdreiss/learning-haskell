module Functors where

    data Point3D a = Point3D a a a deriving Show

    instance Functor Point3D where
        fmap f (Point3D a b c) = Point3D (f a) (f b) (f c)

    data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)

    instance Functor GeomPrimitive where
        fmap f primitive = case primitive of
            Point (Point3D a b c) ->
                Point (Point3D (f a) (f b) (f c))
            LineSegment (Point3D a1 b1 c1) (Point3D a2 b2 c2) ->
                LineSegment (Point3D (f a1) (f b1) (f c1)) (Point3D (f a2) (f b2) (f c2))
