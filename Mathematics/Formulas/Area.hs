module Formulas.Area where
  import Control.Monad (guard)
  import Formulas.Synonyms

  -- 2D Shapes

  getSquareArea :: Length -> Maybe Area
  getSquareArea squareSideLength = do
    guard (squareSideLength > 0)

    return (squareSideLength^2)

  getRectangleArea :: Width -> Height -> Maybe Area
  getRectangleArea rectangleWidth rectangleHeight = do
    guard (rectangleWidth > 0)
    guard (rectangleHeight > 0)

    return (rectangleWidth * rectangleHeight)

  getTriangleArea :: Base -> Height -> Maybe Area
  getTriangleArea triangleBase triangleHeight = do
    guard (triangleBase > 0)
    guard (triangleHeight > 0)

    return ((triangleBase * triangleHeight) / 2)

  getRhombusArea :: Diagonal -> Diagonal -> Maybe Area
  getRhombusArea rhombusLargeDiagonal rhombusSmallDiagonal = do
    guard (rhombusLargeDiagonal > 0)
    guard (rhombusSmallDiagonal > 0)

    return ((rhombusLargeDiagonal * rhombusSmallDiagonal) / 2)

  getTrapezoidArea :: Length -> Length -> Height -> Maybe Area
  getTrapezoidArea trapezoidLargeSide trapezoidSmallSide trapezoidHeight = do
    guard (trapezoidLargeSide > 0)
    guard (trapezoidSmallSide > 0)
    guard (trapezoidHeight > 0)

    return (((trapezoidLargeSide + trapezoidSmallSide) / 2) * trapezoidHeight)

  getApothem :: Length -> NumberOfSides -> Apothem -- Already guarded in getRegularPolygunPerimeter
  getApothem polygonSideLength polygonSideNumber =
    polygonSideLength / (2 * tan(180 / polygonSideNumber))

  getRegularPolygunPerimeter :: Length -> NumberOfSides -> Maybe Perimeter
  getRegularPolygunPerimeter regularPolygonSideLength regularPolygonSideNumber = do
    guard (regularPolygonSideLength > 0)
    guard (regularPolygonSideNumber > 1)

    return (regularPolygonSideLength * regularPolygonSideNumber)

  getRegularPolygonArea :: Length -> NumberOfSides -> Maybe Area
  getRegularPolygonArea regularPolygonPerimeter regularPolygonSideLength regularPolygonSideNumber = do
    regularPolygonPerimeter <- getRegularPolygunPerimeter regularPolygonSideLength regularPolygonSideNumber

    apothem <- getApothem regularPolygonSideLength regularPolygonSideNumber

    return ((regularPolygonPerimeter / 2) * apothem)

  getCirclePerimeter :: Radius -> Maybe Perimeter
  getCirclePerimeter circleRadius = do
    guard (circleRadius > 0)

    return (2 * pi * circleRadius)

  getCircleArea :: Radius -> Maybe Area
  getCircleArea circleRadius = do
    guard (circleRadius > 0)

    return (pi * circleRadius^2)

  -- 3D Shapes

  getConeSlantHeight :: Radius -> Height -> Maybe Height
  getConeSlantHeight coneRadius coneHeight = do
    guard (coneHeight > 0)

    return (sqrt(coneRadius^2 + coneHeight^2))

  getConeSurface :: Radius -> Height -> Maybe Area
  getConeSurface coneRadius coneHeight = do
    guard (coneRadius > 0)

    coneSlantHeight <- getConeSlantHeight coneRadius coneHeight

    return ((pi * r) * coneSlantHeight)

  getSphereSurface :: Radius -> Maybe Area
  getSphereSurface sphereRadius = do
    guard (sphereRadius > 0)

    return (4 * pi * sphereRadius^2)
