data Heap t = Empty
            | Heap t (Heap t) (Heap t) deriving (Show)
            
isEmpty Empty = True
isEmpty _     = False

root Empty = error "Empty heap"
root (Heap v _ _) = v

minLeft cmp Empty                = error "Empty heap"
minLeft cmp (Heap _ Empty Empty) = error "Heap has no children"
minLeft cmp (Heap _ l@Empty r)   = False
minLeft cmp (Heap _ l r@Empty)   = True
minLeft cmp (Heap _ l r)         = cmp (root l) (root r)

heapInsert cmp Empty x        = Heap x Empty Empty
heapInsert cmp (Heap v l r) x = if cmp x v then Heap x r (heapInsert cmp l v)
                                else Heap v r (heapInsert cmp l x)

removeMin cmp Empty                = error "Tried to remove from empty heap"
removeMin cmp (Heap v Empty Empty) = Empty
removeMin cmp h@(Heap v l r)       = if minLeft cmp h
                                     then Heap (root l) (removeMin cmp l) r
                                     else Heap (root r) (removeMin cmp r) l

heapsort cmp xs = let emptyHeap cmp Empty = []
                      emptyHeap cmp h     = root h :
                                            emptyHeap cmp (removeMin cmp h)
                      makeHeap cmp []     = Empty
                      makeHeap cmp (x:xs) = heapInsert cmp (makeHeap cmp xs) x
                  in emptyHeap cmp (makeHeap cmp xs)
