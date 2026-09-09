; MeTTa frontier candidates for Mettaclaw.
; Native port of goal_candidates.py — computes EXPAND / BREAK candidates
; for the LLM goal-setting agent based on graph topology and optionally
; live Hebbian links from AtomSpace.

; ---------------------------------------------------------------------------
; Configuration
; ---------------------------------------------------------------------------

(= (goalCandidatesUseDynamic) False)
(= (goalCandidatesGroupSize) 5)
(= (goalCandidatesHebbianTopPercentile) 0.20)
(= (goalCandidatesHebbianMinWeight) 0.05)

; Toggle boolean-like values (mirrors Python _parse_bool)
(= (goalCandidatesToggle $value)
   (if (== $value False)  False
       (if (== $value false) False
           (if (== $value 0)  False
               (if (== $value "0") False True)))))

; ---------------------------------------------------------------------------
; List helpers
; ---------------------------------------------------------------------------

; First $n elements of $list.
(= (goalCandidatesTake $n ())
   ())
(= (goalCandidatesTake $n $list)
   (if (<= $n 0)
       ()
       (cons-atom (car-atom $list)
                  (goalCandidatesTake (- $n 1) (cdr-atom $list)))))

; Number of elements in a flat list.
(= (goalCandidatesSize $list)
   (if (== $list ()) 0 (+ 1 (goalCandidatesSize (cdr-atom $list)))))

; ---------------------------------------------------------------------------
; Edge accessors — edges from graph space are ((Link src tgt) (mean conf))
; Hebbian edges from getAllHebLinksWithValues are the same shape.
; ---------------------------------------------------------------------------

; Extract all edges from a graph space.
(= (goalCandidatesEdges $graph)
   (collapse (match $graph ($edge $value) ($edge $value))))

(= (goalCandidatesEdgeSource $edge)
   (car-atom (cdr-atom (car-atom $edge))))

(= (goalCandidatesEdgeTarget $edge)
   (car-atom (cdr-atom (cdr-atom (car-atom $edge)))))

(= (goalCandidatesEdgeWeight $edge)
   (let* (
       ($val (cdr-atom $edge))
       ($mean (car-atom $val))
       ($conf (car-atom (cdr-atom $val)))
   )
   (* $mean $conf)))

; Collect all distinct node names that appear in an edge list.
(= (goalCandidatesNodesFromEdges ())
   ())
(= (goalCandidatesNodesFromEdges $edges)
   (if (== $edges ()) ()
       (let* (
           ($head (car-atom $edges))
           ($tail (cdr-atom $edges))
           ($src (goalCandidatesEdgeSource $head))
           ($tgt (goalCandidatesEdgeTarget $head))
           ($rest (goalCandidatesNodesFromEdges $tail))
       )
       (removeDuplicates (cons-atom $src (cons-atom $tgt $rest))))))

; Collect neighbor names of $node from an edge list (undirected).
(= (goalCandidatesNeighborsFromEdges () $node)
   ())
(= (goalCandidatesNeighborsFromEdges $edges $node)
   (if (== $edges ()) ()
       (let* (
           ($head (car-atom $edges))
           ($tail (cdr-atom $edges))
           ($src (goalCandidatesEdgeSource $head))
           ($tgt (goalCandidatesEdgeTarget $head))
           ($rest (goalCandidatesNeighborsFromEdges $tail $node))
       )
       (if (== $src $node)
           (cons-atom $tgt $rest)
               (if (== $tgt $node)
               (cons-atom $src $rest)
               $rest)))))

; Count how many elements of $list are also in $set.
(= (goalCandidatesCountInSet () $set) 0)
(= (goalCandidatesCountInSet $list $set)
   (if (== $list ()) 0
       (let*
           (($head (car-atom $list))
            ($tail (cdr-atom $list))
            ($rest (goalCandidatesCountInSet $tail $set)))
           (if (contains $set $head)
               (+ 1 $rest)
               $rest))))

; Collect elements of $list that are in $set.
(= (goalCandidatesFilterInSet () $set) ())
(= (goalCandidatesFilterInSet $list $set)
   (if (== $list ()) ()
       (let*
           (($head (car-atom $list))
            ($tail (cdr-atom $list))
            ($rest (goalCandidatesFilterInSet $tail $set)))
           (if (contains $set $head)
               (cons-atom $head $rest)
               $rest))))

; Collect elements of $list that are NOT in $set.
(= (goalCandidatesFilterOutSet () $set) ())
(= (goalCandidatesFilterOutSet $list $set)
   (if (== $list ()) ()
       (let*
           (($head (car-atom $list))
            ($tail (cdr-atom $list))
            ($rest (goalCandidatesFilterOutSet $tail $set)))
           (if (contains $set $head)
               $rest
               (cons-atom $head $rest)))))

; ---------------------------------------------------------------------------
; Expand formatting
; ---------------------------------------------------------------------------

; Single entry: (name afCount)
; Returns string "name:countaf"
(= (goalCandidatesFormatOneExpand ())
   "")
(= (goalCandidatesFormatOneExpand $entry)
   (let* (
       ($name (car-atom $entry))
       ($count (car-atom (cdr-atom $entry)))
   )
   (concat (str $name) ":" (str $count) "af")))

; Build space-separated expand string from entries.
(= (goalCandidatesFormatExpand ())
   "none")
(= (goalCandidatesFormatExpand $entries)
   (if (== $entries ()) "none"
       (let* (
           ($head (car-atom $entries))
           ($tail (cdr-atom $entries))
           ($headStr (goalCandidatesFormatOneExpand $head))
           ($restStr (goalCandidatesFormatExpand $tail))
       )
       (if (== $restStr "none")
           $headStr
           (concat $headStr " " $restStr)))))

; ---------------------------------------------------------------------------
; Break formatting
; ---------------------------------------------------------------------------

; Single entry: (name degree)
; Returns string "name:dDegree"
(= (goalCandidatesFormatOneBreak ())
   "")
(= (goalCandidatesFormatOneBreak $entry)
   (let* (
       ($name (car-atom $entry))
       ($degree (car-atom (cdr-atom $entry)))
   )
   (concat (str $name) ":d" (str $degree))))

; Build space-separated break string from entries.
(= (goalCandidatesFormatBreak ())
   "none")
(= (goalCandidatesFormatBreak $entries)
   (if (== $entries ()) "none"
       (let* (
           ($head (car-atom $entries))
           ($tail (cdr-atom $entries))
           ($headStr (goalCandidatesFormatOneBreak $head))
           ($restStr (goalCandidatesFormatBreak $tail))
       )
       (if (== $restStr "none")
           $headStr
           (concat $headStr " " $restStr)))))

; ---------------------------------------------------------------------------
; Expand scoring — entries are (name afCount)
; Sort key: -afCount (number) for descending by afCount.
; NOTE: MeTTa < >= only work on Numbers, not tuples, so secondary
; tiebreak by name is not possible via sortAtomsDescending.
; ---------------------------------------------------------------------------

(= (goalCandidatesExpandScore ($name $afCount))
   (- 0 $afCount))

; ---------------------------------------------------------------------------
; Break scoring — entries are (name degree)
; Sort key: degree (number) for ascending by degree.
; ---------------------------------------------------------------------------

(= (goalCandidatesBreakScore ($name $degree))
   $degree)

; Break fallback scoring — entries are (name afConn)
; Sort key: afConn (number) for ascending by AF connectivity.
; ---------------------------------------------------------------------------

(= (goalCandidatesBreakFallbackScore ($name $afConn))
   $afConn)

; ---------------------------------------------------------------------------
; STATIC MODE
; ---------------------------------------------------------------------------

(= (goalCandidatesStatic $afAtoms $graph $groupSize)
   (let* (
       ($size (if (== $groupSize ()) (goalCandidatesGroupSize) $groupSize))
       ($af $afAtoms)
       ($edges (goalCandidatesEdges $graph))
       ($allNodes (goalCandidatesNodesFromEdges $edges))
       ($outside (goalCandidatesFilterOutSet $allNodes $af))

       ; EXPAND: outside nodes with at least one AF neighbor
       ($expandRaw
           (collapse
               (map-atom $outside $node
                   (let* (
                       ($neigh (goalCandidatesNeighborsFromEdges $edges $node))
                       ($afNeigh (goalCandidatesFilterInSet $neigh $af))
                       ($count (goalCandidatesSize $afNeigh))
                   )
                   (if (> $count 0)
                       (cons-atom $node (cons-atom $count ()))
                       ())))))

       ; Sort expand entries descending by afCount
       ($expandSorted
           (sortAtomsDescending $expandRaw goalCandidatesExpandScore))

       ($expandTop (goalCandidatesTake $size $expandSorted))
       ($expandStr (goalCandidatesFormatExpand $expandTop))

       ; BREAK: outside nodes with zero AF neighbors
       ($breakRaw
           (collapse
               (map-atom $outside $node
                   (let* (
                       ($neigh (goalCandidatesNeighborsFromEdges $edges $node))
                       ($afNeigh (goalCandidatesFilterInSet $neigh $af))
                       ($afCount (goalCandidatesSize $afNeigh))
                       ($degree (goalCandidatesSize $neigh))
                   )
                   (if (== $afCount 0)
                       (cons-atom $node (cons-atom $degree ()))
                       ())))))

       ; Sort break entries ascending by degree
       ($breakSorted
           (sortAtomsDescending $breakRaw goalCandidatesBreakScore))

       ($breakTop (goalCandidatesTake $size $breakSorted))
       ($breakStr (goalCandidatesFormatBreak $breakTop))
   )
   (concat "EXPAND(out-AF, adjacent) " $expandStr
           " | BREAK(out-AF, distant) " $breakStr)))

; ---------------------------------------------------------------------------
; DYNAMIC MODE
; ---------------------------------------------------------------------------

(= (goalCandidatesDynamic $afAtoms $hebbianLinks $graph $groupSize)
   (let* (
       ($size (if (== $groupSize ()) (goalCandidatesGroupSize) $groupSize))
       ($af $afAtoms)
       ($staticEdges (goalCandidatesEdges $graph))
       ($hebbianEdges $hebbianLinks)

       ; Threshold Hebbian links: keep top percentile by weight, min 0.05
       ($sortedHebbian
           (sortAtomsDescending $hebbianEdges goalCandidatesEdgeWeight))
       ($hebbianCount (goalCandidatesSize $sortedHebbian))
       ($topCount (ceil (* $hebbianCount (goalCandidatesHebbianTopPercentile))))
       ($strongHebbianRaw (goalCandidatesTake $topCount $sortedHebbian))
       ($strongHebbian
           (collapse
               (map-atom $strongHebbianRaw $edge
                   (if (>= (goalCandidatesEdgeWeight $edge)
                           (goalCandidatesHebbianMinWeight))
                       $edge
                       ()))))

       ; All known nodes
       ($staticNodes (goalCandidatesNodesFromEdges $staticEdges))
       ($hebbianNodes (goalCandidatesNodesFromEdges $strongHebbian))
       ($allNodes (removeDuplicates (concatTuple $staticNodes $hebbianNodes)))
       ($outside (goalCandidatesFilterOutSet $allNodes $af))

       ; EXPAND: outside nodes with static AF neighbors OR hebbian AF neighbors
       ; Score = staticCount*1.0 + hebbCount*2.0 + hebbWeight
       ; Entry format: (name staticAfCount hebbAfCount score)
       ($expandRaw
           (collapse
               (map-atom $outside $node
                   (let* (
                       ($staticNeigh (goalCandidatesNeighborsFromEdges $staticEdges $node))
                       ($hebbNeigh (goalCandidatesNeighborsFromEdges $strongHebbian $node))
                       ($staticAfCount (goalCandidatesCountInSet $staticNeigh $af))
                       ($hebbAfCount (goalCandidatesCountInSet $hebbNeigh $af))
                       ($hebbAfNodes (goalCandidatesFilterInSet $hebbNeigh $af))
                       ; Sum hebbian weights to AF neighbors
                       ($hebbWeight
                           (foldl-atom $hebbAfNodes 0 $acc $n
                               (+ $acc
                                  (let ($w
                                      (goalCandidatesEdgeWeight
                                          (goalCandidatesFindEdge $strongHebbian $node $n)))
                                      (if (== $w ()) 0.0 $w)))))
                       ($score (+ (+ (* $staticAfCount 1.0)
                                    (* $hebbAfCount 2.0))
                                 $hebbWeight))
                   )
                   (if (or (> $staticAfCount 0) (> $hebbAfCount 0))
                       (cons-atom $node
                           (cons-atom $staticAfCount
                               (cons-atom $hebbAfCount
                                   (cons-atom $score ()))))
                       ())))))

       ; Sort expand by score descending (score is 4th element)
       ($expandSorted
           (sortAtomsDescending $expandRaw goalCandidatesExpandDynScore))

       ($expandTop (goalCandidatesTake $size $expandSorted))
       ($expandStr (goalCandidatesFormatExpandDyn $expandTop))

       ; BREAK: outside nodes with ZERO AF connections in both static and hebbian
       ($breakRaw
           (collapse
               (map-atom $outside $node
                   (let* (
                       ($staticNeigh (goalCandidatesNeighborsFromEdges $staticEdges $node))
                       ($hebbNeigh (goalCandidatesNeighborsFromEdges $strongHebbian $node))
                       ($staticAfCount (goalCandidatesCountInSet $staticNeigh $af))
                       ($hebbAfCount (goalCandidatesCountInSet $hebbNeigh $af))
                       ($degree (+ (goalCandidatesSize $staticNeigh)
                                   (goalCandidatesSize $hebbNeigh)))
                   )
                   (if (and (== $staticAfCount 0) (== $hebbAfCount 0))
                       (cons-atom $node (cons-atom $degree ()))
                       ())))))

        ; Sort break ascending by degree
        ($breakSorted
            (sortAtomsDescending $breakRaw goalCandidatesBreakScore))

        ($breakTop (goalCandidatesTake $size $breakSorted))

        ; Fallback: if no isolated nodes, pick nodes with lowest AF connectivity
        ($breakStr
            (if (== $breakTop ())
                (let* (
                    ($fallbackRaw
                        (collapse
                            (map-atom $outside $node
                                (let* (
                                    ($staticNeigh (goalCandidatesNeighborsFromEdges $staticEdges $node))
                                    ($hebbNeigh (goalCandidatesNeighborsFromEdges $strongHebbian $node))
                                    ($staticAfCount (goalCandidatesCountInSet $staticNeigh $af))
                                    ($hebbAfCount (goalCandidatesCountInSet $hebbNeigh $af))
                                    ($afConn (+ $staticAfCount $hebbAfCount))
                                )
                                (cons-atom $node (cons-atom $afConn ()))))))
                    ($fallbackSorted
                        (sortAtomsDescending $fallbackRaw goalCandidatesBreakFallbackScore))
                    ($fallbackTop (goalCandidatesTake $size $fallbackSorted))
                )
                (goalCandidatesFormatBreakFallback $fallbackTop))
                (goalCandidatesFormatBreak $breakTop)))
    )
    (concat "EXPAND(out-AF, dynamic-adj) " $expandStr
            " | BREAK(out-AF, isolated) " $breakStr)))

; ---------------------------------------------------------------------------
; Break fallback formatting (used when no isolated nodes found)
; Entry format: (name afConn) -- sorted ascending by afConn
; ---------------------------------------------------------------------------

(= (goalCandidatesFormatOneBreakFallback ())
   "")
(= (goalCandidatesFormatOneBreakFallback $entry)
   (let* (
       ($name (car-atom $entry))
       ($afConn (car-atom (cdr-atom $entry)))
   )
   (concat (str $name) ":c" (str $afConn))))

(= (goalCandidatesFormatBreakFallback ())
   "none")
(= (goalCandidatesFormatBreakFallback $entries)
   (if (== $entries ()) "none"
       (let* (
           ($head (car-atom $entries))
           ($tail (cdr-atom $entries))
           ($headStr (goalCandidatesFormatOneBreakFallback $head))
           ($restStr (goalCandidatesFormatBreakFallback $tail))
       )
       (if (== $restStr "none")
           $headStr
           (concat $headStr " " $restStr)))))

; ---------------------------------------------------------------------------
; Dynamic expand helpers
; ---------------------------------------------------------------------------

; Find an edge between $src and $tgt in an edge list (undirected).
(= (goalCandidatesFindEdge () $src $tgt) ())
(= (goalCandidatesFindEdge $edges $src $tgt)
   (if (== $edges ()) ()
       (let* (
           ($head (car-atom $edges))
           ($tail (cdr-atom $edges))
           ($eSrc (goalCandidatesEdgeSource $head))
           ($eTgt (goalCandidatesEdgeTarget $head))
       )
       (if (or (and (== $eSrc $src) (== $eTgt $tgt))
               (and (== $eSrc $tgt) (== $eTgt $src)))
           $head
           (goalCandidatesFindEdge $tail $src $tgt)))))

; Dynamic expand entry: (name staticAfCount hebbAfCount score)
; Sort key: -score (number) for descending by score.
(= (goalCandidatesExpandDynScore ($name $staticAf $hebbAf $score))
   (- 0 $score))

; Format dynamic expand entry: "name:Naf+Mheb" or "name:Naf"
(= (goalCandidatesFormatOneExpandDyn ())
   "")
(= (goalCandidatesFormatOneExpandDyn $entry)
   (let* (
       ($name (car-atom $entry))
       ($staticAf (car-atom (cdr-atom $entry)))
       ($hebbAf (car-atom (cdr-atom (cdr-atom $entry))))
   )
   (if (> $hebbAf 0)
       (concat (str $name) ":" (str $staticAf) "af+" (str $hebbAf) "heb")
       (concat (str $name) ":" (str $staticAf) "af"))))

(= (goalCandidatesFormatExpandDyn ())
   "none")
(= (goalCandidatesFormatExpandDyn $entries)
   (if (== $entries ()) "none"
       (let* (
           ($head (car-atom $entries))
           ($tail (cdr-atom $entries))
           ($headStr (goalCandidatesFormatOneExpandDyn $head))
           ($restStr (goalCandidatesFormatExpandDyn $tail))
       )
       (if (== $restStr "none")
           $headStr
           (concat $headStr " " $restStr)))))

; ---------------------------------------------------------------------------
; Entry point
; ---------------------------------------------------------------------------

(= (goalCandidates $afAtoms $hebbianLinks $graph $groupSize $useDynamic)
   (if (goalCandidatesToggle $useDynamic)
       (goalCandidatesDynamic $afAtoms $hebbianLinks $graph $groupSize)
       (goalCandidatesStatic $afAtoms $graph $groupSize)))
