-------------------------------------------------------------------------------
-- DO NOT MODIFY THIS SEGMENT
-------------------------------------------------------------------------------

module BHeap where

import qualified Test.QuickCheck as QC

-- | Binary heap datatype
data BHeap = Leaf                       -- ^ Empty heap 
           | Node Int Bool BHeap BHeap  -- ^ Node with a value, whether this heap is full,
                                        -- ^ and left and right subheap
  deriving (Eq, Show)

-- | Does the heap contain a given element?
-- | (Assuming the heap is valid)
contains :: Int -> BHeap -> Bool
contains _ Leaf           = False
contains x (Node y _ l r)
  | x == y    = True
  | x < y     = contains x l || contains x r
  | otherwise = False

-- | Height of the heap
height :: BHeap -> Int
height Leaf           = 0
height (Node _ _ l r) = 1 + max (height l) (height r)

-- | Is this binary heap full according to the cached value?
isFull :: BHeap -> Bool
isFull Leaf              = True
isFull (Node _ full _ _) = full

-- | Is this binary heap actually full?
-- | We say that a heap is full if has no partially filled levels.
isFullSlow :: BHeap -> Bool
isFullSlow Leaf           = True
isFullSlow (Node _ _ l r) = isFullSlow l && isFullSlow r && (height l == height r)

-------------------------------------------------------------------------------
-- Task 2.1: Efficient insert
-------------------------------------------------------------------------------

-- | `insert x h`: insert `x` into the binary heap `h`
-- | (You can assume `h` is a valid heap)
insert :: Int -> BHeap -> BHeap
insert = error "TBD..."

-------------------------------------------------------------------------------
-- Task 2.2: Build with HOF
-------------------------------------------------------------------------------

-- | Insert all values from a list into a binary heap
build :: [Int] -> BHeap
build = error "TBD..."

-------------------------------------------------------------------------------
-- Task 2.3: Checking shape property
-------------------------------------------------------------------------------

-- | Does this binary tree satisfy the shape property of the heap?
isHeapShape :: BHeap -> Bool
isHeapShape = error "TBD..."

-------------------------------------------------------------------------------
-- Task 2.4: Checking value property
-------------------------------------------------------------------------------

-- | Does this binary tree satisfy the value property of the heap?
isHeapValue :: BHeap -> Bool
isHeapValue = error "TBD..."

-------------------------------------------------------------------------------
-- Task 2.5: Checking elements property
-------------------------------------------------------------------------------

-- | Does `h` contain all the elements of `xs`?
containsAll :: [Int] -> BHeap -> Bool
containsAll = error "TBD..."

-------------------------------------------------------------------------------
-- | QuickCheck: DO NOT MODIFY THIS SEGMENT
-------------------------------------------------------------------------------

-- | A heap built from `xs` accurately caches whether it is full
prop_full_correct :: [Int] -> Bool
prop_full_correct xs = let h = build xs in isFull h == isFullSlow h

-- | A heap built from `xs` satisfies the shape property
prop_is_heap_shape :: [Int] -> Bool
prop_is_heap_shape xs = let h = build xs in isHeapShape h

-- | A heap built from `xs` satisfies the value property
prop_is_heap_value :: [Int] -> Bool
prop_is_heap_value xs = let h = build xs in isHeapValue h

-- | A heap built from `xs` contains all `xs`
prop_contains_elts :: [Int] -> Bool
prop_contains_elts xs = let h = build xs in containsAll xs h

quickCheck :: (QC.Testable prop) => prop -> IO ()
quickCheck = QC.quickCheck
