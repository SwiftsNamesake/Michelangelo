-- |
-- Module      : Southpaw.Michelangelo.Utils
-- Description : Utilities for doing 3D graphics with OpenGL
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
-- 

-- Created July 20 2015
-- Adapted from http://hackage.haskell.org/package/scenegraph-0.1.0.1/docs/src/Graphics-SceneGraph-Textures.html#getAndCreateTextures

-- TODO | - 
--        - 

-- SPEC | -
--        -



module Southpaw.Michelangelo.Utils where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
-- import Graphics.UI.GLUT
-- import Graphics.SceneGraph.ReadImage (readImage)
-- import Graphics.SceneGraph.TGA
import Codec.Picture
import Control.Monad (when)
import Data.Word
-- import Foreign.Marshal.Alloc

import qualified Data.ByteString as BS



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- |
saveImage :: FilePath -> BS.ByteString -> IO ()
saveImage fn image = BS.writeFile fn image


-- | 
-- createTexture :: Size -> PixelData Int -> IO (Maybe TextureObject) 
-- createTexture (Size cx cy) pixels = do
-- 	[tex] <- genObjectNames 1 --
-- 	textureBinding Texture2D $= Just tex --
-- 	build2DMipmaps Texture2D RGBA' (fromIntegral cx) (fromIntegral cy) pixels
-- 	textureFilter Texture2D $= ((Linear', Just Nearest), Linear')
-- 	textureFunction $= Modulate
-- 	-- free ptr
-- 	return $ Just tex


-- |
-- createTextures :: [(Size, PixelData Int)] -> IO [Maybe TextureObject]
-- createTextures fn = return 


-- -- read a list of images and returns a list of textures
-- -- all images are assumed to be in the TGA image format
-- getAndCreateTextures :: [String] -> IO [Maybe TextureObject]
-- getAndCreateTextures fileNames = do
--    fileNamesExts <- return (map (++".tga") fileNames)
--    texData <- mapM readImageC fileNamesExts
--    texObjs <- mapM createTexture texData
--    return texObjs


-- -- read a single texture
-- getAndCreateTexture :: String -> IO (Maybe TextureObject)
-- getAndCreateTexture fileName = do
--    texData <- readImageC (fileName++".tga")
--    texObj <- createTexture texData
--    return texObj


-- -- read the image data
-- readImageC :: String -> IO (Maybe (Size, PixelData Word8))
-- readImageC path = catch (readTga path) (\err -> do
--    print ("missing texture: "++path)
--    return Nothing)


-- -- creates the texture
-- createTexture :: (Maybe (Size, PixelData a)) -> IO (Maybe TextureObject)
-- createTexture (Just ((Size x y), pixels@(PixelData t1 t2 ptr))) = do
--    [texName] <- genObjectNames 1  -- generate our texture.
--    --rowAlignment  Unpack $= 1
--    textureBinding Texture2D $= Just texName  -- make our new texture the current texture.
--    --generateMipmap Texture2D $= Enabled
--    build2DMipmaps Texture2D RGBA' (fromIntegral x) (fromIntegral y) pixels
--    textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')
--    --textureWrapMode Texture2D S $= (Repeated, Repeat)
--    --textureWrapMode Texture2D T $= (Repeated, Repeat)
--    textureFunction $= Modulate
--    free ptr
--    return (Just texName)
-- createTexture Nothing = return Nothing
