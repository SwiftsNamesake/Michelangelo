-- |
-- Module      : Southpaw.Michelangelo.Transformations
-- Description : Matrix and quaternion operations
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
-- 

-- Created July 31 2015

-- TODO | - 
--        - 

-- SPEC | -
--        -



module Southpaw.Michelangelo.Transformations where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Linear.Matrix
import Linear.V3
import Linear.Quaternion
import Linear.Epsilon
import Linear.Projection

-- import Control.Lens



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- |
rotateX :: (Floating n, Epsilon n) => n -> M44 n
rotateX  = m33_to_m44 . fromQuaternion . axisAngle (V3 1 0 0)


-- |
rotateY :: (Floating n, Epsilon n) => n -> M44 n
rotateY  = m33_to_m44 . fromQuaternion . axisAngle (V3 0 1 0)


-- |
rotateZ :: (Floating n, Epsilon n) => n -> M44 n
rotateZ  = m33_to_m44 . fromQuaternion . axisAngle (V3 0 0 1)