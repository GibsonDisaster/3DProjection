module Main where
  import Graphics.Gloss.Interface.IO.Animate
  import Data.Matrix

  yRotationValue :: Float
  yRotationValue = 0.5

  rotX :: Float -> Matrix Float
  rotX θ = fromList 3 3 [
                          1, 0, 0,
                          0, cos θ, negate (sin θ),
                          0, sin θ, cos θ
                        ]

  rotY :: Float -> Matrix Float
  rotY θ = fromList 3 3 [
                          cos θ, 0, negate (sin θ),
                          0, 1, 1,
                          sin θ, 0, cos θ
                        ]

  rotZ :: Float -> Matrix Float
  rotZ θ = fromList 3 3 [
                          cos θ, negate (sin θ), 0,
                          sin θ, cos θ, 0,
                          0, 0, 1
                        ]

  projection :: Matrix Float
  projection = fromList 2 3 [
                              1, 0, 0,
                              0, 1, 0
                            ]

  tuplify :: [a] -> (a, a)
  tuplify [x, y] = (x, y)

  pointToMatrix :: (Float, Float, Float) -> Matrix Float
  pointToMatrix (x, y, z) = fromList 3 1 [x, y, z]

  matrixToPoint :: Matrix Float -> (Float, Float)
  matrixToPoint m = tuplify $ toList m

  cubePoints3D :: [(Float, Float, Float)]
  cubePoints3D = [
                  (50, 50, 50), -- first square top right corner
                  (50, (-50), 50),
                  ((-50), (-50), 50),
                  ((-50), 50, 50),
                  (50, 50, (-50)), -- second square top right corner
                  (50, (-50), (-50)),
                  ((-50), (-50), (-50)),
                  ((-50), 50, (-50))
                ]

  pyramidPoints3D :: [(Float, Float, Float)]
  pyramidPoints3D = [
                      (200, 0, 0),
                      (300, 0, 0),
                      (200, 0, 100),
                      (300, 0, 100),
                      (250, 200, 50)
                    ]

  -- PYRAMID

  pyramidPointsRotated :: Float -> [Matrix Float]
  pyramidPointsRotated t = map ((*) (rotY yRotationValue)) xRotated
    where
      pyramidPoints = map pointToMatrix cubePoints3D
      xRotated = map ((*) (rotX t)) pyramidPoints

  pyramidPoints2D :: Float -> [Matrix Float]
  pyramidPoints2D t = map ((*) projection) (pyramidPointsRotated t)

  rotatedPyramidPointsFromMats :: Float -> [(Float, Float)]
  rotatedPyramidPointsFromMats t = map matrixToPoint (pyramidPoints2D t)

  -- CUBE

  cubePointsRotated :: Float -> [Matrix Float]
  cubePointsRotated t = map ((*) (rotY yRotationValue)) xRotated
    where
      cubePoints = map pointToMatrix cubePoints3D
      xRotated = map ((*) (rotX t)) cubePoints

  cubePoints2D :: Float -> [Matrix Float]
  cubePoints2D t = map ((*) projection) (cubePointsRotated t)

  rotatedPointsFromMats :: Float -> [(Float, Float)]
  rotatedPointsFromMats t = map matrixToPoint (cubePoints2D t)

  -- render functions

  makeSquare :: Int -> [(Float, Float)] -> Picture
  makeSquare n pts = if n == 1 then color blue (lineLoop pts) else color red (lineLoop pts)

  makeLines :: [(Float, Float)] -> Picture
  makeLines pts = Pictures [color white $ line a, color white $ line b, color white $ line c, color white $ line d]
    where
      a = [pts !! 0, pts !! 4]
      b = [pts !! 1, pts !! 5]
      c = [pts !! 2, pts !! 6]
      d = [pts !! 3, pts !! 7]

  connectToTip :: [(Float, Float)] -> Picture
  connectToTip coords = Pictures [color white $ line a, color white $ line b, color white $ line c, color white $ line d]
    where
      a = [coords !! 0, last coords]
      b = [coords !! 1, last coords]
      c = [coords !! 2, last coords]
      d = [coords !! 3, last coords]

  renderCube :: Float -> IO Picture
  renderCube t = return $ Pictures [makeSquare 1 firstSqr, makeSquare 2 secondSqr, makeLines points] -- ++ [makeSquare 1 pyramidBase, connectToTip (rotatedPyramidPointsFromMats yRotationValue t)]
    where
      points = rotatedPointsFromMats t
      firstSqr = take 4 points
      secondSqr = drop 4 points
      pyramidBase = take 4 $ rotatedPyramidPointsFromMats t

  main :: IO ()
  main = animateIO (InWindow "3D Projection" (800, 800) (240, 360)) black renderCube (\c -> return ())