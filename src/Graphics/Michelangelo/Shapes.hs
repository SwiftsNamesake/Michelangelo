-- |
-- Module      : Graphics.Michelangelo.Shapes
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability :
--

-- Created July 30 2015

-- TODO | - Generic container types (eg. Monoids or OverloadedLists?)
--        - Generic vertex types (?)
--        - Anchored shapes (?)
--        - Bounding boxes
--        - Indexed shapes
--        - Triangulation (or - more generally - tiling) of polygons
--        - QuickCheck
--        - Performance
--        - Types to represent surfaces, edges, etc.
--        - Different coordinate systems and orientations (eg. clockwise vs counter-clockwise)
--        - Generating colours (eg. monochrome) and creating data that is easily uploaded to the GPU

-- SPEC | -
--        -



module Graphics.Michelangelo.Shapes where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
-- import Data.Monoid

import Graphics.Michelangelo.Types



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------

-- | A 'Face' is a list of vertices
-- TODO: Polymorphic container
-- TODO: Make sure all vertices lie in the same plane (using Triangles?)
data Face v = Face [v]


-- |
data Edge v = Edge v v



--------------------------------------------------------------------------------------------------------------------------------------------
-- Data
--------------------------------------------------------------------------------------------------------------------------------------------

-- | TODO: Factor out
π :: Floating f => f
π = pi



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- Vertex constructors ---------------------------------------------------------------------------------------------------------------------

-- | A simple list of vertices
vlist :: a -> a -> a -> [a]
vlist x y z = [x, y, z]

-- Tessellation ----------------------------------------------------------------------------------------------------------------------------

-- | Tessellate a polygon with triangles. Presently, only concave shapes will tessellate properly.
-- TODO: Make it work for any shape (convex and concave)
triangles :: [a] -> [[a]]
triangles (a:rest) = pairwise (\b c -> [a, b, c]) rest
  where
    -- | Combine every adjacent pair in the list with the given function
    pairwise :: (a -> a -> b) -> [a] -> [b]
    pairwise f xs = zipWith f xs (drop 1 xs)

-- Two-dimensional shapes ------------------------------------------------------------------------------------------------------------------

-- | Generate the vertices for a rectangular plane, centred at (0,0,0)
plane :: Fractional f => (f -> f -> f -> a) -> f -> f -> f -> [a]
plane f dx dy dz = [f (-dx/2) (dy/2) (dz/2), f (dx/2) (dy/2) (dz/2), f (dx/2) (-dy/2) (-dz/2), f (-dx/2) (-dy/2) (-dz/2)]


-- | Generate the vertices for a rectangular plane, parallel with the X and Y axes and centred at (0,0,0)
-- TODO: General 'plane' function
planeXY :: Fractional f => (f -> f -> f -> a) -> f -> f -> [a]
planeXY f dx dy = plane f dx dy 0
-- planeXY dx dy = [f (-dx/2) (dy/2) 0, f (dx/2) (dy/2) 0, f (dx/2) (-dy/2) 0, f (-dx/2) (-dy/2) 0]


-- | Generate the vertices for a rectangular plane, parallel with the X and Z axes and centred at (0,0,0)
-- TODO: General 'plane' function
planeXZ :: Fractional f => (f -> f -> f -> a) -> f -> f -> [a]
planeXZ f dx dz = plane f dx 0 dz


-- | Generate the vertices for a rectangular plane, parallel with the Y and Z axes and centred at (0,0,0)
-- TODO: General 'plane' function
planeYZ :: Fractional f => (f -> f -> f -> a) -> f -> f -> [a]
planeYZ f dy dz = plane f 0 dy dz


-- | Generate the vertices for a regular polygon, centred at (0,0,0)
polygon :: (Floating f, Integral i) => (f -> f -> a) -> i -> f -> [a]
polygon f sides radius = [ let θ = fromIntegral n * 2*π/fromIntegral sides in f (radius*cos θ) (radius*sin θ) | n <- [0..(sides-1)] ]

-- Three-dimensional shapes ----------------------------------------------------------------------------------------------------------------

-- | Generate the vertices of an axis-aligned cuboid centred at (0,0,0)
-- TODO: Use combinatorics to generate vertices (?)
cuboid :: Fractional f => (f -> f -> f -> a) -> f -> f -> f -> [a]
cuboid f dx dy dz = [f (-hdx)   hdy  hdz, f hdx   hdy  hdz, f hdx   hdy  (-hdz), f (-hdx)   hdy  (-hdz), --
                     f (-hdx) (-hdy) hdz, f hdx (-hdy) hdz, f hdx (-hdy) (-hdz), f (-hdx) (-hdy) (-hdz)] --
  where
    (hdx, hdy, hdz) = (dx/2, dy/2, dz/2)


-- | Generate the vertices of an axis-aligned cube centred at (0,0,0)
cube :: Fractional f => (f -> f -> f -> a) -> f -> [a]
cube f side = cuboid f side side side

-- Indices ---------------------------------------------------------------------------------------------------------------------------------

-- | Indices for a solid cuboid (with triangulated sides, cf. `cuboid`)
-- TODO: Use more sensible container types (three levels of lists seems a bit excessive...)
-- TODO: Allow any kind of tessellation (?)
cuboidIndices :: Integral i => [[[i]]]
cuboidIndices = map triangles [[0,1,2,3], [4,5,6,7], [3,2,6,7], [0,1,5,4], [0,3,7,4], [1,2,6,5]] -- Top, bottom, Front, Back, Left, Right

--------------------------------------------------------------------------------------------------------------------------------------------

-- | Indices for a shape extruded from a 2D polygon
-- TODO: Figure out which types to use
-- TODO: Generic 'stitching' of lid and bottom, counter-clockwise or clockwise (?)
-- TODO: Generate vertices and indices separately
-- extrude :: (Vector v, Fractional f) => (v -> v') -> v -> [v] -> [v']
-- extrude :: (Num v, Fractional f) => (v -> v') -> v -> [v] -> [v']
-- extrude f direction shape = undefined
--   where
--     -- |
--     -- side ::
--     side a b = concat [[f a, f $ a + direction, f $ b + direction], [f a, f $ b + direction, f b]]
--
--     -- | Stitches together two polygons by successively connecting one edge from each shape with two adjacent triangles.
--     --   The end result is a rim joining the shapes, made out of a triangle strip.
--     -- TODO: Factor out 'looping' (ie. closing the loop)
--     -- Found hole `_pairwise' with type: (v -> v -> [v']) -> [(a1, b)] -> t0 [a]
--     stitch side' bottom lid = concat . pairwise (uncurry side) $ zip (close bottom) (close lid)
--
--     -- |
--     -- TODO: I wonder how many times I've written this function (factor out)
--     pairwise f xs = zipWith f xs (tail xs)
--
--     -- |
--     close shape' = shape' ++ [head shape']
--
--     -- |
--     rim = stitch undefined
