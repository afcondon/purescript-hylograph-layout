-- | Geometric Layout Patterns
-- |
-- | Simple, deterministic layout functions for positioning N items within a viewport.
-- | Each function handles all the tedious calculations (aspect ratio, spacing, centering)
-- | and returns an array of points ready to zip with your data.
-- |
-- | ## Usage
-- |
-- | ```purescript
-- | import DataViz.Layout.Pattern (grid, phyllotaxis)
-- | import DataViz.Layout.Pattern.Types (viewportWithPadding)
-- |
-- | -- Layout 100 items in a grid with 20px padding
-- | let vp = viewportWithPadding 800.0 600.0 20.0
-- | let positions = grid vp 100
-- |
-- | -- Zip with your data
-- | let positioned = Array.zipWith (\pos datum -> { x: pos.x, y: pos.y, datum })
-- |                                positions myData
-- | ```
module DataViz.Layout.Pattern
  ( -- * Grid layouts
    grid
  , gridWithAspect
  , gridExact

  -- * Offset layouts
  , honeycomb
  , brick

  -- * Spiral layouts
  , phyllotaxis
  , archimedean
  , fermat

  -- * Circular layouts
  , radial
  , concentricRings
  , sunburst

  -- * Linear layouts
  , horizontal
  , vertical
  , diagonal
  , along

  -- * Layered layouts (for neural networks, etc.)
  , layered
  , layeredCentered

  -- * Masonry layouts
  , masonry
  , masonryAuto

  -- * Shelf (flow/wrap) layouts
  , shelf
  , shelfUniform

  -- * Waffle chart
  , waffle

  -- * Stacked bar layouts
  , stacked
  , stackedHorizontal
  , stackedDiverging

  -- * Waterfall chart
  , waterfall

  -- * Justified (image gallery) layout
  , justified

  -- * Bin packing
  , binPack

  -- * Calendar grid
  , calendarGrid

  -- * Swimlane
  , swimlane

  -- * Re-exports
  , module Types
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, sum)
import Data.Int (floor, toNumber, ceil)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (sqrt, pi, cos, sin, abs)
import DataViz.Layout.Pattern.Types (Point, Rect, Viewport, WaffleCell, usableArea)
import DataViz.Layout.Pattern.Types (Point, Rect, Viewport, Padding, WaffleCell, viewport, viewportWithPadding, usableArea, uniformPadding, padding, noPadding) as Types

-- =============================================================================
-- Grid Layouts
-- =============================================================================

-- | Lay out N items in a grid, automatically choosing rows/columns
-- | to best fit the viewport's aspect ratio.
-- |
-- | Items are centered within the viewport and evenly spaced.
grid :: Viewport -> Int -> Array Point
grid vp n =
  let
    area = usableArea vp
    aspect = area.width / area.height

    -- Choose columns to match aspect ratio
    -- cols/rows ≈ aspect, and cols * rows ≥ n
    cols = max 1 $ ceil $ sqrt (toNumber n * aspect)
    rows = max 1 $ ceil (toNumber n / toNumber cols)

    -- Cell dimensions
    cellW = area.width / toNumber cols
    cellH = area.height / toNumber rows

    -- Center items within cells
    offsetX = cellW / 2.0
    offsetY = cellH / 2.0
  in
    Array.mapWithIndex
      (\i _ ->
        let
          col = i `mod` cols
          row = i / cols
        in
          { x: area.x + toNumber col * cellW + offsetX
          , y: area.y + toNumber row * cellH + offsetY
          }
      )
      (Array.replicate n unit)

-- | Grid with a specific target aspect ratio for cells (width/height)
gridWithAspect :: Number -> Viewport -> Int -> Array Point
gridWithAspect cellAspect vp n =
  let
    area = usableArea vp
    viewAspect = area.width / area.height

    -- Solve for cols where cells have target aspect ratio
    -- cellW/cellH = cellAspect
    -- cellW = width/cols, cellH = height/rows
    -- rows = ceil(n/cols)
    -- So: (width/cols) / (height/ceil(n/cols)) = cellAspect
    cols = max 1 $ ceil $ sqrt (toNumber n * viewAspect / cellAspect)
    rows = max 1 $ ceil (toNumber n / toNumber cols)

    cellW = area.width / toNumber cols
    cellH = area.height / toNumber rows
    offsetX = cellW / 2.0
    offsetY = cellH / 2.0
  in
    Array.mapWithIndex
      (\i _ ->
        let
          col = i `mod` cols
          row = i / cols
        in
          { x: area.x + toNumber col * cellW + offsetX
          , y: area.y + toNumber row * cellH + offsetY
          }
      )
      (Array.replicate n unit)

-- | Grid with exact column count specified
gridExact :: Int -> Viewport -> Int -> Array Point
gridExact cols vp n =
  let
    area = usableArea vp
    rows = max 1 $ ceil (toNumber n / toNumber (max 1 cols))
    actualCols = max 1 cols

    cellW = area.width / toNumber actualCols
    cellH = area.height / toNumber rows
    offsetX = cellW / 2.0
    offsetY = cellH / 2.0
  in
    Array.mapWithIndex
      (\i _ ->
        let
          col = i `mod` actualCols
          row = i / actualCols
        in
          { x: area.x + toNumber col * cellW + offsetX
          , y: area.y + toNumber row * cellH + offsetY
          }
      )
      (Array.replicate n unit)

-- =============================================================================
-- Offset Layouts (Honeycomb, Brick)
-- =============================================================================

-- | Honeycomb layout - offset alternating rows for hexagonal packing
-- | More compact than a regular grid for circular items.
honeycomb :: Viewport -> Int -> Array Point
honeycomb vp n =
  let
    area = usableArea vp
    aspect = area.width / area.height

    -- Hex packing: rows are offset by half, vertical spacing is sqrt(3)/2 of horizontal
    verticalRatio = sqrt 3.0 / 2.0

    cols = max 1 $ ceil $ sqrt (toNumber n * aspect / verticalRatio)
    rows = max 1 $ ceil (toNumber n / toNumber cols)

    cellW = area.width / toNumber cols
    cellH = area.height / toNumber rows

    offsetX = cellW / 2.0
    offsetY = cellH / 2.0
  in
    Array.mapWithIndex
      (\i _ ->
        let
          col = i `mod` cols
          row = i / cols
          -- Offset odd rows by half a cell width
          rowOffset = if row `mod` 2 == 1 then cellW / 2.0 else 0.0
        in
          { x: area.x + toNumber col * cellW + offsetX + rowOffset
          , y: area.y + toNumber row * cellH + offsetY
          }
      )
      (Array.replicate n unit)

-- | Brick layout - like honeycomb but with rectangular cells
-- | Good for text labels or rectangular items.
brick :: Viewport -> Int -> Array Point
brick = honeycomb  -- Same algorithm, different typical use case

-- =============================================================================
-- Spiral Layouts
-- =============================================================================

-- | Golden angle constant (radians)
goldenAngle :: Number
goldenAngle = pi * (3.0 - sqrt 5.0)

-- | Phyllotaxis layout - golden angle spiral like sunflower seeds
-- | Produces beautiful, naturally-looking distributions.
-- | Items are placed at increasing distances from center using golden angle.
phyllotaxis :: Viewport -> Int -> Array Point
phyllotaxis vp n =
  let
    area = usableArea vp
    centerX = area.x + area.width / 2.0
    centerY = area.y + area.height / 2.0

    -- Scale to fit viewport (use smaller dimension to ensure fit)
    maxRadius = min (area.width / 2.0) (area.height / 2.0)

    -- Spacing factor - sqrt(i) gives even area distribution
    scale = maxRadius / sqrt (toNumber n)
  in
    Array.mapWithIndex
      (\i _ ->
        let
          -- Radius grows with sqrt of index for even area distribution
          r = scale * sqrt (toNumber i)
          -- Angle increases by golden angle each step
          theta = toNumber i * goldenAngle
        in
          { x: centerX + r * cos theta
          , y: centerY + r * sin theta
          }
      )
      (Array.replicate n unit)

-- | Archimedean spiral - constant spacing between arms
archimedean :: Number -> Viewport -> Int -> Array Point
archimedean spacing vp n =
  let
    area = usableArea vp
    centerX = area.x + area.width / 2.0
    centerY = area.y + area.height / 2.0
    maxRadius = min (area.width / 2.0) (area.height / 2.0)

    -- Total angle for n points
    totalAngle = toNumber n * spacing
    radiusPerRadian = maxRadius / totalAngle
  in
    Array.mapWithIndex
      (\i _ ->
        let
          theta = toNumber i * spacing
          r = theta * radiusPerRadian
        in
          { x: centerX + r * cos theta
          , y: centerY + r * sin theta
          }
      )
      (Array.replicate n unit)

-- | Fermat spiral - like phyllotaxis but with configurable divergence angle
fermat :: Number -> Viewport -> Int -> Array Point
fermat divergenceAngle vp n =
  let
    area = usableArea vp
    centerX = area.x + area.width / 2.0
    centerY = area.y + area.height / 2.0
    maxRadius = min (area.width / 2.0) (area.height / 2.0)
    scale = maxRadius / sqrt (toNumber n)
  in
    Array.mapWithIndex
      (\i _ ->
        let
          r = scale * sqrt (toNumber i)
          theta = toNumber i * divergenceAngle
        in
          { x: centerX + r * cos theta
          , y: centerY + r * sin theta
          }
      )
      (Array.replicate n unit)

-- =============================================================================
-- Circular Layouts
-- =============================================================================

-- | Radial layout - items evenly distributed around a circle
radial :: Viewport -> Int -> Array Point
radial vp n =
  let
    area = usableArea vp
    centerX = area.x + area.width / 2.0
    centerY = area.y + area.height / 2.0
    radius = min (area.width / 2.0) (area.height / 2.0) * 0.9  -- 90% to leave margin

    angleStep = 2.0 * pi / toNumber (max 1 n)
  in
    Array.mapWithIndex
      (\i _ ->
        let
          theta = toNumber i * angleStep - pi / 2.0  -- Start from top
        in
          { x: centerX + radius * cos theta
          , y: centerY + radius * sin theta
          }
      )
      (Array.replicate n unit)

-- | Concentric rings - distribute items across multiple rings
-- | Inner rings have fewer items proportional to their circumference.
concentricRings :: Int -> Viewport -> Int -> Array Point
concentricRings ringCount vp n =
  let
    area = usableArea vp
    centerX = area.x + area.width / 2.0
    centerY = area.y + area.height / 2.0
    maxRadius = min (area.width / 2.0) (area.height / 2.0) * 0.9

    -- Distribute items proportionally to ring circumference
    -- Ring k has radius (k+1)/ringCount * maxRadius
    -- Circumference proportional to radius, so items ∝ (k+1)
    totalWeight = toNumber $ (ringCount * (ringCount + 1)) / 2

    -- Calculate how many items per ring
    itemsPerRing = Array.mapWithIndex
      (\k _ ->
        let weight = toNumber (k + 1) / totalWeight
        in max 1 $ floor (toNumber n * weight + 0.5)
      )
      (Array.replicate ringCount unit)

    -- Generate points for each ring
    generateRing :: Int -> Int -> Int -> Array Point
    generateRing ringIdx _startIdx count =
      let
        radius = maxRadius * toNumber (ringIdx + 1) / toNumber ringCount
        angleStep = 2.0 * pi / toNumber (max 1 count)
      in
        Array.mapWithIndex
          (\i _ ->
            let theta = toNumber i * angleStep - pi / 2.0
            in { x: centerX + radius * cos theta
               , y: centerY + radius * sin theta
               }
          )
          (Array.replicate count unit)
  in
    -- Flatten all rings, take exactly n items
    Array.take n $ Array.concat $
      Array.mapWithIndex
        (\ringIdx count -> generateRing ringIdx 0 count)
        itemsPerRing

-- | Sunburst layout - like concentric rings but with angular sectors
sunburst :: Int -> Viewport -> Int -> Array Point
sunburst = concentricRings  -- Alias for discoverability

-- =============================================================================
-- Linear Layouts
-- =============================================================================

-- | Horizontal line across the viewport
horizontal :: Viewport -> Int -> Array Point
horizontal vp n =
  let
    area = usableArea vp
    centerY = area.y + area.height / 2.0
    step = if n <= 1 then 0.0 else area.width / toNumber (n - 1)
  in
    Array.mapWithIndex
      (\i _ -> { x: area.x + toNumber i * step, y: centerY })
      (Array.replicate n unit)

-- | Vertical line down the viewport
vertical :: Viewport -> Int -> Array Point
vertical vp n =
  let
    area = usableArea vp
    centerX = area.x + area.width / 2.0
    step = if n <= 1 then 0.0 else area.height / toNumber (n - 1)
  in
    Array.mapWithIndex
      (\i _ -> { x: centerX, y: area.y + toNumber i * step })
      (Array.replicate n unit)

-- | Diagonal from top-left to bottom-right
diagonal :: Viewport -> Int -> Array Point
diagonal vp n =
  let
    area = usableArea vp
    stepX = if n <= 1 then 0.0 else area.width / toNumber (n - 1)
    stepY = if n <= 1 then 0.0 else area.height / toNumber (n - 1)
  in
    Array.mapWithIndex
      (\i _ ->
        { x: area.x + toNumber i * stepX
        , y: area.y + toNumber i * stepY
        }
      )
      (Array.replicate n unit)

-- | Layout along an arbitrary line between two points
along :: Point -> Point -> Int -> Array Point
along start end n =
  let
    dx = end.x - start.x
    dy = end.y - start.y
    step = if n <= 1 then 0.0 else 1.0 / toNumber (n - 1)
  in
    Array.mapWithIndex
      (\i _ ->
        let t = toNumber i * step
        in { x: start.x + t * dx
           , y: start.y + t * dy
           }
      )
      (Array.replicate n unit)

-- =============================================================================
-- Layered Layouts
-- =============================================================================

-- | Layered layout - multiple horizontal rows with varying item counts.
-- | Perfect for neural networks, hierarchies, or any row-based visualization.
-- |
-- | Takes an array of item counts per layer. Returns flat array of all positions.
-- | Layers are evenly spaced vertically; items within each layer are evenly spaced
-- | horizontally across the full width.
-- |
-- | ```purescript
-- | -- Neural network: 4 inputs, 8 hidden, 8 hidden, 2 outputs
-- | let positions = layered vp [4, 8, 8, 2]
-- | ```
layered :: Viewport -> Array Int -> Array Point
layered vp layerCounts =
  let
    area = usableArea vp
    numLayers = Array.length layerCounts

    -- Vertical spacing between layers
    layerStep = if numLayers <= 1
                then 0.0
                else area.height / toNumber (numLayers - 1)

    -- Generate positions for one layer
    layerPositions :: Int -> Int -> Array Point
    layerPositions layerIdx count =
      let
        y = area.y + toNumber layerIdx * layerStep
        step = if count <= 1 then 0.0 else area.width / toNumber (count - 1)
      in
        Array.mapWithIndex
          (\i _ -> { x: area.x + toNumber i * step, y })
          (Array.replicate count unit)
  in
    Array.concat $ Array.mapWithIndex layerPositions layerCounts

-- | Layered layout with items centered in each row.
-- | Unlike `layered`, this centers items using cell-based positioning
-- | (items are centered within evenly-divided cells, not edge-to-edge).
-- |
-- | Better for when you want consistent margins at row edges.
layeredCentered :: Viewport -> Array Int -> Array Point
layeredCentered vp layerCounts =
  let
    area = usableArea vp
    numLayers = Array.length layerCounts

    -- Vertical spacing - use cells, center items within
    layerHeight = area.height / toNumber (max 1 numLayers)

    -- Generate positions for one layer
    layerPositions :: Int -> Int -> Array Point
    layerPositions layerIdx count =
      let
        y = area.y + (toNumber layerIdx + 0.5) * layerHeight
        cellWidth = area.width / toNumber (max 1 count)
      in
        Array.mapWithIndex
          (\i _ -> { x: area.x + (toNumber i + 0.5) * cellWidth, y })
          (Array.replicate count unit)
  in
    Array.concat $ Array.mapWithIndex layerPositions layerCounts

-- =============================================================================
-- Masonry Layouts
-- =============================================================================

-- | Masonry (Pinterest-style) layout with fixed column count.
-- | Places each item into the shortest column, producing a compact arrangement.
-- |
-- | Takes column count, gap between items, viewport, and array of item heights.
-- | Returns positioned rectangles in input order.
-- |
-- | ```purescript
-- | let rects = masonry 3 10.0 (viewport 800.0 600.0) [100.0, 150.0, 80.0, 200.0, 120.0]
-- | ```
masonry :: Int -> Number -> Viewport -> Array Number -> Array Rect
masonry cols gap vp heights =
  let
    area = usableArea vp
    actualCols = max 1 cols
    colWidth = (area.width - gap * toNumber (actualCols - 1)) / toNumber actualCols

    -- Find index of shortest column
    shortestCol :: Array Number -> Int
    shortestCol colHeights =
      let
        indexed = Array.mapWithIndex (\i h -> { i, h }) colHeights
      in
        fromMaybe 0 $ map _.i $ foldl
          (\acc item -> case acc of
            Nothing -> Just item
            Just best -> if item.h < best.h then Just item else Just best
          )
          Nothing
          indexed

    -- Accumulator: column heights and collected rects
    result = foldl
      (\acc itemHeight ->
        let
          col = shortestCol acc.colHeights
          x = area.x + toNumber col * (colWidth + gap)
          colH = fromMaybe 0.0 (Array.index acc.colHeights col)
          -- Add gap if column already has items
          y = area.y + colH + (if colH > 0.0 then gap else 0.0)
          rect = { x, y, width: colWidth, height: itemHeight }
          newColHeight = (y - area.y) + itemHeight
          newColHeights = fromMaybe acc.colHeights $
            Array.updateAt col newColHeight acc.colHeights
        in
          { colHeights: newColHeights
          , rects: acc.rects <> [ rect ]
          }
      )
      { colHeights: Array.replicate actualCols 0.0
      , rects: []
      }
      heights
  in
    result.rects

-- | Masonry layout with automatic column count from max column width.
-- | Computes how many columns fit given the max width, then delegates to `masonry`.
-- |
-- | ```purescript
-- | let rects = masonryAuto 250.0 10.0 (viewport 800.0 600.0) heights
-- | ```
masonryAuto :: Number -> Number -> Viewport -> Array Number -> Array Rect
masonryAuto maxColWidth gap vp heights =
  let
    area = usableArea vp
    -- Solve: cols * maxColWidth + (cols - 1) * gap <= area.width
    -- cols * (maxColWidth + gap) <= area.width + gap
    cols = max 1 $ floor ((area.width + gap) / (maxColWidth + gap))
  in
    masonry cols gap vp heights

-- =============================================================================
-- Shelf (Flow/Wrap) Layouts
-- =============================================================================

-- | Shelf layout — place items left-to-right, wrapping to the next row on overflow.
-- | Horizontal complement to masonry. Each item has its own width and height.
-- |
-- | ```purescript
-- | let rects = shelf 8.0 (viewport 800.0 600.0) [{width: 120.0, height: 80.0}, ...]
-- | ```
shelf :: Number -> Viewport -> Array { width :: Number, height :: Number } -> Array Rect
shelf gap vp items =
  let
    area = usableArea vp

    result = foldl
      (\acc item ->
        let
          -- Wrap to next row if this item overflows
          needsWrap = acc.x + item.width > area.x + area.width && acc.x > area.x
          x = if needsWrap then area.x else acc.x
          y = if needsWrap then acc.y + acc.rowHeight + gap else acc.y
          rowH = if needsWrap then item.height else max acc.rowHeight item.height
          rect = { x, y, width: item.width, height: item.height }
          nextX = x + item.width + gap
        in
          { x: nextX
          , y
          , rowHeight: rowH
          , rects: acc.rects <> [ rect ]
          }
      )
      { x: area.x
      , y: area.y
      , rowHeight: 0.0
      , rects: []
      }
      items
  in
    result.rects

-- | Shelf layout with uniform item height — only widths vary.
-- |
-- | ```purescript
-- | let rects = shelfUniform 40.0 8.0 (viewport 800.0 600.0) [100.0, 150.0, 80.0]
-- | ```
shelfUniform :: Number -> Number -> Viewport -> Array Number -> Array Rect
shelfUniform itemHeight gap vp widths =
  shelf gap vp (map (\w -> { width: w, height: itemHeight }) widths)

-- =============================================================================
-- Waffle Chart
-- =============================================================================

-- | Waffle chart — proportional unit grid (alternative to pie charts).
-- | A rows×cols grid where cells are colored by category proportional to each
-- | category's count. Cells filled left-to-right, top-to-bottom.
-- |
-- | ```purescript
-- | let cells = waffle 10 10 (viewport 400.0 400.0) [30, 25, 20, 15, 10]
-- | ```
waffle :: Int -> Int -> Viewport -> Array Int -> Array WaffleCell
waffle rows cols vp categoryCounts =
  let
    area = usableArea vp
    totalCells = max 1 rows * max 1 cols
    actualRows = max 1 rows
    actualCols = max 1 cols
    cellW = area.width / toNumber actualCols
    cellH = area.height / toNumber actualRows

    -- Expand category counts to flat array of category indices
    -- e.g. [3, 2] → [0, 0, 0, 1, 1]
    expandCategories :: Array Int -> Array Int
    expandCategories counts =
      Array.concat $ Array.mapWithIndex
        (\catIdx count -> Array.replicate count catIdx)
        counts

    categories = Array.take totalCells (expandCategories categoryCounts)
  in
    Array.mapWithIndex
      (\i catIdx ->
        let
          col = i `mod` actualCols
          row = i / actualCols
          rect = { x: area.x + toNumber col * cellW
                 , y: area.y + toNumber row * cellH
                 , width: cellW
                 , height: cellH
                 }
        in
          { rect, category: catIdx }
      )
      categories

-- =============================================================================
-- Stacked Bar Layouts
-- =============================================================================

-- | Vertical stacked bar chart — groups of segments stacked bottom-to-top.
-- | Each inner array is one bar's segments. All bars scaled to the same maximum.
-- |
-- | ```purescript
-- | let rects = stacked 8.0 (viewport 800.0 400.0) [[30.0, 20.0, 10.0], [25.0, 35.0, 5.0]]
-- | ```
stacked :: Number -> Viewport -> Array (Array Number) -> Array (Array Rect)
stacked gap vp groups =
  let
    area = usableArea vp
    numGroups = Array.length groups
    barWidth = if numGroups == 0 then 0.0
               else (area.width - gap * toNumber (max 0 (numGroups - 1))) / toNumber numGroups

    -- Find max group total for scaling
    maxTotal = foldl (\acc g -> max acc (sum g)) 0.0 groups
    scale = if maxTotal == 0.0 then 0.0 else area.height / maxTotal
  in
    Array.mapWithIndex
      (\groupIdx segments ->
        let
          barX = area.x + toNumber groupIdx * (barWidth + gap)
          -- Stack segments from bottom up
          result = foldl
            (\acc segValue ->
              let
                segHeight = segValue * scale
                y = acc.y - segHeight
                rect = { x: barX, y, width: barWidth, height: segHeight }
              in
                { y, rects: acc.rects <> [ rect ] }
            )
            { y: area.y + area.height  -- Start from bottom
            , rects: []
            }
            segments
        in
          result.rects
      )
      groups

-- | Horizontal stacked bar chart — segments stacked left-to-right.
stackedHorizontal :: Number -> Viewport -> Array (Array Number) -> Array (Array Rect)
stackedHorizontal gap vp groups =
  let
    area = usableArea vp
    numGroups = Array.length groups
    barHeight = if numGroups == 0 then 0.0
                else (area.height - gap * toNumber (max 0 (numGroups - 1))) / toNumber numGroups

    maxTotal = foldl (\acc g -> max acc (sum g)) 0.0 groups
    scale = if maxTotal == 0.0 then 0.0 else area.width / maxTotal
  in
    Array.mapWithIndex
      (\groupIdx segments ->
        let
          barY = area.y + toNumber groupIdx * (barHeight + gap)
          result = foldl
            (\acc segValue ->
              let
                segWidth = segValue * scale
                rect = { x: acc.x, y: barY, width: segWidth, height: barHeight }
              in
                { x: acc.x + segWidth, rects: acc.rects <> [ rect ] }
            )
            { x: area.x
            , rects: []
            }
            segments
        in
          result.rects
      )
      groups

-- | Diverging stacked bar chart — segments extend in both directions from a center baseline.
-- | Positive values go right/up from center, negative go left/down.
stackedDiverging :: Number -> Viewport -> Array (Array Number) -> Array (Array Rect)
stackedDiverging gap vp groups =
  let
    area = usableArea vp
    numGroups = Array.length groups
    barWidth = if numGroups == 0 then 0.0
               else (area.width - gap * toNumber (max 0 (numGroups - 1))) / toNumber numGroups

    -- Find max extent in each direction for scaling
    maxExtent = foldl
      (\acc segments ->
        let
          result = foldl
            (\innerAcc v ->
              if v >= 0.0
                then innerAcc { pos = innerAcc.pos + v }
                else innerAcc { neg = innerAcc.neg + abs v }
            )
            { pos: 0.0, neg: 0.0 }
            segments
        in
          { pos: max acc.pos result.pos, neg: max acc.neg result.neg }
      )
      { pos: 0.0, neg: 0.0 }
      groups

    totalExtent = maxExtent.pos + maxExtent.neg
    scale = if totalExtent == 0.0 then 0.0 else area.height / totalExtent
    -- Baseline Y: proportion of negative extent from top
    baselineY = area.y + maxExtent.neg * scale
  in
    Array.mapWithIndex
      (\groupIdx segments ->
        let
          barX = area.x + toNumber groupIdx * (barWidth + gap)
          result = foldl
            (\acc v ->
              if v >= 0.0 then
                let
                  segHeight = v * scale
                  y = acc.posY - segHeight
                  rect = { x: barX, y, width: barWidth, height: segHeight }
                in
                  acc { posY = y, rects = acc.rects <> [ rect ] }
              else
                let
                  segHeight = abs v * scale
                  rect = { x: barX, y: acc.negY, width: barWidth, height: segHeight }
                in
                  acc { negY = acc.negY + segHeight, rects = acc.rects <> [ rect ] }
            )
            { posY: baselineY, negY: baselineY, rects: [] }
            segments
        in
          result.rects
      )
      groups

-- =============================================================================
-- Waterfall Chart
-- =============================================================================

-- | Waterfall chart — cumulative bars where each starts where the previous ended.
-- | Positive deltas go up, negative deltas go down.
-- |
-- | ```purescript
-- | let rects = waterfall 40.0 8.0 (viewport 800.0 400.0) [100.0, 50.0, -30.0, 80.0, -20.0]
-- | ```
waterfall :: Number -> Number -> Viewport -> Array Number -> Array Rect
waterfall barWidth gap vp deltas =
  let
    area = usableArea vp
    n = Array.length deltas

    -- Compute running totals via scanl
    runningTotals = foldl
      (\acc d ->
        let prev = fromMaybe 0.0 (Array.last acc)
        in acc <> [ prev + d ]
      )
      [ 0.0 ]
      deltas

    -- Find min/max for scaling
    minVal = foldl min 0.0 runningTotals
    maxVal = foldl max 0.0 runningTotals
    range = maxVal - minVal
    scale = if range == 0.0 then 0.0 else area.height / range

    -- Convert value to Y coordinate (higher values = lower Y in screen space)
    valueToY :: Number -> Number
    valueToY v = area.y + (maxVal - v) * scale

    -- Actual bar width accounting for total space
    actualBarWidth = min barWidth
      (if n == 0 then 0.0
       else (area.width - gap * toNumber (max 0 (n - 1))) / toNumber n)
  in
    Array.mapWithIndex
      (\i delta ->
        let
          prevTotal = fromMaybe 0.0 (Array.index runningTotals i)
          currTotal = prevTotal + delta
          x = area.x + toNumber i * (actualBarWidth + gap)
          top = min prevTotal currTotal
          bottom = max prevTotal currTotal
          y = valueToY bottom
          h = (bottom - top) * scale
        in
          { x, y, width: actualBarWidth, height: max 0.0 h }
      )
      deltas

-- =============================================================================
-- Justified (Image Gallery) Layout
-- =============================================================================

-- | Justified layout — all rows fill exact viewport width, row heights vary.
-- | Like Google Images or Flickr galleries. Input is aspect ratios (width/height).
-- |
-- | Algorithm: greedily fill rows. Each row's height = availableWidth / sum(aspectRatios).
-- | When height drops below threshold, break to next row. Last row left-aligned at target height.
-- |
-- | ```purescript
-- | let rects = justified 200.0 8.0 (viewport 800.0 600.0) [1.5, 0.75, 1.33, 2.0, 0.8]
-- | ```
justified :: Number -> Number -> Viewport -> Array Number -> Array Rect
justified targetRowHeight gap vp aspectRatios =
  let
    area = usableArea vp
    availableWidth = area.width
    minRowHeight = targetRowHeight * 0.6

    -- Build rows greedily
    buildRows :: Array Number -> Array (Array { aspect :: Number, index :: Int })
    buildRows aspects =
      let
        indexed = Array.mapWithIndex (\i a -> { aspect: a, index: i }) aspects
        result = foldl
          (\acc item ->
            let
              currentRow = acc.currentRow <> [ item ]
              sumAspects = foldl (\s it -> s + it.aspect) 0.0 currentRow
              rowH = (availableWidth - gap * toNumber (max 0 (Array.length currentRow - 1))) / sumAspects
            in
              if rowH < minRowHeight && Array.length currentRow > 1 then
                -- Row too short, break before this item
                { rows: acc.rows <> [ acc.currentRow ]
                , currentRow: [ item ]
                }
              else
                acc { currentRow = currentRow }
          )
          { rows: [], currentRow: [] }
          indexed
      in
        -- Add final row
        if Array.length result.currentRow > 0
          then result.rows <> [ result.currentRow ]
          else result.rows

    rows = buildRows aspectRatios

    -- Layout each row
    layoutRow :: Number -> Array { aspect :: Number, index :: Int } -> Boolean
              -> { y :: Number, rects :: Array { rect :: Rect, index :: Int } }
              -> { y :: Number, rects :: Array { rect :: Rect, index :: Int } }
    layoutRow rowY row isLast acc =
      let
        sumAspects = foldl (\s it -> s + it.aspect) 0.0 row
        gapTotal = gap * toNumber (max 0 (Array.length row - 1))
        rowH = if isLast
               then targetRowHeight  -- Last row: use target height
               else (availableWidth - gapTotal) / sumAspects

        rowRects = foldl
          (\inner item ->
            let
              w = if isLast
                  then item.aspect * rowH
                  else item.aspect * rowH
              rect = { x: inner.x, y: rowY, width: w, height: rowH }
            in
              { x: inner.x + w + gap
              , items: inner.items <> [ { rect, index: item.index } ]
              }
          )
          { x: area.x, items: [] }
          row
      in
        { y: rowY + rowH + gap
        , rects: acc.rects <> rowRects.items
        }

    -- Fold over all rows
    numRows = Array.length rows
    laidOut = foldl
      (\acc rowWithIdx ->
        let isLast = rowWithIdx.idx == numRows - 1
        in layoutRow acc.y rowWithIdx.row isLast acc
      )
      { y: area.y, rects: [] }
      (Array.mapWithIndex (\idx row -> { idx, row }) rows)

    -- Sort back to original order
    sorted = Array.sortWith _.index laidOut.rects
  in
    map _.rect sorted

-- =============================================================================
-- Bin Packing (Skyline Bottom-Left)
-- =============================================================================

-- | 2D rectangle bin packing using Skyline Bottom-Left algorithm.
-- | Packs rectangles into the viewport like a texture atlas / sprite sheet.
-- | Items are sorted tallest-first for better packing.
-- |
-- | ```purescript
-- | let rects = binPack (viewport 800.0 600.0) [{width: 100.0, height: 80.0}, ...]
-- | ```
binPack :: Viewport -> Array { width :: Number, height :: Number } -> Array Rect
binPack vp items =
  let
    area = usableArea vp

    -- Sort items tallest-first, preserving original indices
    indexed = Array.mapWithIndex (\i item -> { item, origIdx: i }) items
    sorted = Array.sortBy (\a b -> compare b.item.height a.item.height) indexed

    -- Skyline: array of { x, y, segWidth } segments representing top edge
    initialSkyline = [ { x: area.x, y: area.y, segWidth: area.width } ]

    -- Find best position for an item on the skyline (leftmost, lowest)
    findPosition skyline itemW _itemH =
      foldl
        (\best segWithIdx ->
          let
            seg = segWithIdx.seg
            -- Check if item fits starting at this segment
            fits = seg.x + itemW <= area.x + area.width
            -- Find max Y across segments this item would span
            maxY = if fits then findMaxY skyline seg.x itemW else 0.0
          in
            if fits then
              case best of
                Nothing -> Just { x: seg.x, y: maxY, segIdx: segWithIdx.idx }
                Just b ->
                  if maxY < b.y || (maxY == b.y && seg.x < b.x)
                    then Just { x: seg.x, y: maxY, segIdx: segWithIdx.idx }
                    else best
            else best
        )
        Nothing
        (Array.mapWithIndex (\idx seg -> { seg, idx }) skyline)

    -- Find max Y across skyline segments that an item at position x with width w would overlap
    findMaxY skyline x w =
      foldl
        (\maxH seg ->
          let
            segRight = seg.x + seg.segWidth
            itemRight = x + w
            overlaps = seg.x < itemRight && segRight > x
          in
            if overlaps then max maxH seg.y else maxH
        )
        area.y
        skyline

    -- Update skyline after placing an item
    updateSkyline skyline px py pw ph =
      let
        newY = py + ph
        itemRight = px + pw

        -- Split/trim segments that overlap with placed item
        updated = Array.concat $ map
          (\seg ->
            let
              segRight = seg.x + seg.segWidth
              overlaps = seg.x < itemRight && segRight > px
            in
              if overlaps then
                let
                  -- Left remainder
                  leftPart = if seg.x < px
                    then [ { x: seg.x, y: seg.y, segWidth: px - seg.x } ]
                    else []
                  -- Right remainder
                  rightPart = if segRight > itemRight
                    then [ { x: itemRight, y: seg.y, segWidth: segRight - itemRight } ]
                    else []
                in
                  leftPart <> [ { x: max seg.x px, y: newY, segWidth: min segRight itemRight - max seg.x px } ] <> rightPart
              else
                [ seg ]
          )
          skyline

        -- Merge adjacent segments at same height
        merged = foldl
          (\acc seg ->
            case Array.last acc of
              Just prev | prev.y == seg.y && prev.x + prev.segWidth == seg.x ->
                fromMaybe acc $ do
                  init <- Array.init acc
                  pure $ init <> [ prev { segWidth = prev.segWidth + seg.segWidth } ]
              _ -> acc <> [ seg ]
          )
          []
          (Array.sortWith _.x updated)
      in
        if Array.length merged == 0 then initialSkyline else merged

    -- Pack all items
    packResult = foldl
      (\acc entry ->
        case findPosition acc.skyline entry.item.width entry.item.height of
          Nothing ->
            -- Doesn't fit; place at bottom of current skyline
            let y = foldl (\m seg -> max m seg.y) area.y acc.skyline
            in acc { placed = acc.placed <> [ { rect: { x: area.x, y, width: entry.item.width, height: entry.item.height }, origIdx: entry.origIdx } ]
                   , skyline = updateSkyline acc.skyline area.x y entry.item.width entry.item.height
                   }
          Just pos ->
            acc { placed = acc.placed <> [ { rect: { x: pos.x, y: pos.y, width: entry.item.width, height: entry.item.height }, origIdx: entry.origIdx } ]
                , skyline = updateSkyline acc.skyline pos.x pos.y entry.item.width entry.item.height
                }
      )
      { skyline: initialSkyline, placed: [] }
      sorted

    -- Re-sort by original index
    reordered = Array.sortWith _.origIdx packResult.placed
  in
    map _.rect reordered

-- =============================================================================
-- Calendar Grid
-- =============================================================================

-- | Calendar grid — 7-column weekly layout with weekday offset.
-- | Pure geometry: caller converts dates to day-of-week offset and day count.
-- |
-- | ```purescript
-- | -- March 2026 starts on Sunday (offset 0), has 31 days
-- | let rects = calendarGrid 0 31 4.0 (viewport 700.0 500.0)
-- | ```
calendarGrid :: Int -> Int -> Number -> Viewport -> Array Rect
calendarGrid startDayOfWeekOffset numDays gap vp =
  let
    area = usableArea vp
    offset = max 0 (min 6 startDayOfWeekOffset)
    totalWeeks = max 1 $ ceil (toNumber (offset + numDays) / 7.0)
    cellW = (area.width - gap * 6.0) / 7.0
    cellH = (area.height - gap * toNumber (totalWeeks - 1)) / toNumber totalWeeks
  in
    Array.mapWithIndex
      (\i _ ->
        let
          daySlot = offset + i
          col = daySlot `mod` 7
          row = daySlot / 7
        in
          { x: area.x + toNumber col * (cellW + gap)
          , y: area.y + toNumber row * (cellH + gap)
          , width: cellW
          , height: cellH
          }
      )
      (Array.replicate numDays unit)

-- =============================================================================
-- Swimlane Layout
-- =============================================================================

-- | Swimlane layout — horizontal lanes with items positioned by start/end.
-- | For timelines, Gantt charts, kanban boards.
-- | Start and end values are normalized 0.0-1.0, mapped to viewport width.
-- |
-- | ```purescript
-- | let rects = swimlane 3 4.0 (viewport 800.0 300.0)
-- |               [ {lane: 0, start: 0.0, end: 0.4}
-- |               , {lane: 1, start: 0.2, end: 0.8}
-- |               , {lane: 2, start: 0.5, end: 1.0}
-- |               ]
-- | ```
swimlane :: Int -> Number -> Viewport -> Array { lane :: Int, start :: Number, end :: Number } -> Array Rect
swimlane numLanes gap vp items =
  let
    area = usableArea vp
    actualLanes = max 1 numLanes
    laneHeight = (area.height - gap * toNumber (max 0 (actualLanes - 1))) / toNumber actualLanes
  in
    map
      (\item ->
        let
          lane = max 0 (min (actualLanes - 1) item.lane)
          y = area.y + toNumber lane * (laneHeight + gap)
          x = area.x + item.start * area.width
          w = (item.end - item.start) * area.width
        in
          { x, y, width: max 0.0 w, height: laneHeight }
      )
      items
