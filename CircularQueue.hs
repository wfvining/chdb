-- | A fast-enough circular queue...
module CircularQueue
       ( empty
       , next
       , peek
       , insert
       , toCircularQueue
       , CircularQueue(..)
       ) where

data CircularQueue a = CQ [a] [a] deriving (Show)

-- | an empty circular queue
empty :: CircularQueue a
empty = CQ [] []

-- | the head of the queue.
peek :: CircularQueue a -> a
peek (CQ (x:_) _) = x

-- | a tuple containing the next element in the queue, and the new queue.
next :: CircularQueue a -> (a, CircularQueue a)
next (CQ [] [])     = error "empty queue"
next (CQ [] xs)     = next (CQ (reverse xs) [])
next (CQ (x:xs) ys) = (x, CQ xs (x:ys))

-- | insert an element into the queue.
insert :: a -> CircularQueue a -> CircularQueue a
insert x (CQ xs ys) = CQ xs (x:ys)

-- | remove the head of the queue.
remove :: CircularQueue a -> (a, CircularQueue a)
remove (CQ [] [])     = error "empty queue"
remove (CQ [] ys)     = remove $ CQ (reverse ys) []
remove (CQ (x:xs) ys) = (x, CQ xs ys)

-- | create a queue containing all the elements in a list.
toCircularQueue :: [a] -> CircularQueue a
toCircularQueue = CQ []
