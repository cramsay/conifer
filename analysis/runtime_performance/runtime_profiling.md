## Run test

Running pickHcub 10 times for N=10 b=11.

Better determinism from a fixed (not random) set of set of weights and by running single-threaded


```
ghc TestProfile.hs -O2 -fexcess-precision -optc-O3 -optc-ffast-math -fforce-recomp  -prof -fprof-auto -rtsopts

./TestProfile  +RTS -p

cp TestProfile.prof ../../profiling/Run_$(git log --oneline | head -n 1 | cut -f1 -d' ').prof
```

Golden run returns "10, 12.7, 5.8, 311.0"

First attempt runs in 44.18 seconds, and here's the top 5 culprits...

```
        total time  =       44.18 secs   (44179 ticks @ 1000 us, 1 processor)
        total alloc = 65,028,276,608 bytes  (excludes profiling overheads)

COST CENTRE            MODULE     SRC                             %time %alloc

aStarSet               Graph.Aop  Graph/Aop.hs:(79,3)-(94,8)       28.9   20.1
succSetSingle.addFund  Graph.Util Graph/Util.hs:74:7-56             8.7   11.6
aOp                    Graph.Aop  Graph/Aop.hs:(38,1)-(42,14)       7.5    4.8
applySign              Graph.Aop  Graph/Aop.hs:(21,1)-(22,22)       7.1    2.3
isDistance2.case1      Graph.Util Graph/Util.hs:140:7-40            6.7    5.8
aStarSet.unshiftedVal  Graph.Aop  Graph/Aop.hs:89:14-55             5.4    4.0
```

Not that if I use abs of the A-operation output, I don't have to call aStarSet twice with the arguments swapped!
Let's try that...

V good.

Note that hcub.bBar accounts for 99% of the time
Inside this, 
  distRst uses 73% and 
  distRt  uses 27%
  
distRst : 
  estimateDistance uses 38%,
    isDistance 2 uses 32%
    distance 3 is barely anything! but distance 2 isn't. Why? Partly because it's called 30 times less XD
  incrementalSuccSet is another 34%

Pain points in distRt look similar.

Note that distEst function is super slowed down by E3 which calls estCSD a suspicious amount.

> Probably because that's the Astar(c1*s,t) case... and I don't check what's still within the valid range!
> Add a range check and look for similar things in the code.

Looks like DistRt is called 0.5 times the number DistRst is called. This should be a bigger disparity!

> Make distRt a map of ts to distRt values in the let binding. It would be hard
> to reuse otherwise because we select an s by iterating through it at the top
> level.

Super! That's distRt down to ~1% from 27%! Great.

distRst is now 98.6% of the (11) run-time...
This is split pretty evenly between estimating distance and doing the incremental successor set

I should implement the tricks in the hcub paper (bottom of page 24)

I should implement the tricks in the hcub paper (bottom of page 24) While
calculating dist(R,t), we need to find intersections with S. We can use these
intersections to determine *which* successors have dist(R+s,t)=dist(R,t)-1 and
which just have dist(R,t). Therefore, for exact distance test, we only need to
calculate dist(R,t) for all T and then save some by-products.

For the distance estimates, start from dist(R,t) = maxBound and only bother
calculating dist(R+s,t) for all S and T. If s is chosen, update dist(R,t) caches
with all dist(R,t) for that s.

How is this affected by the optimal stage? There might be successors added
between estimate distance calls...

Great! That helped so much. Now facing bottlenecks in CSD implementations!

```
	total time  =        3.57 secs   (3566 ticks @ 1000 us, 1 processor)
	total alloc = 6,590,971,528 bytes  (excludes profiling overheads)

COST CENTRE           MODULE     SRC                             %time %alloc

csdCost.nonZeros      Graph.CSD  Graph/CSD.hs:35:9-44             29.2    6.6
toCSD.go              Graph.CSD  Graph/CSD.hs:(21,5)-(31,31)      14.9   20.3
toBits                Graph.CSD  Graph/CSD.hs:8:1-57              14.6   35.7
toCSD                 Graph.CSD  Graph/CSD.hs:(19,1)-(31,31)      13.3   23.0
csdCost               Graph.CSD  Graph/CSD.hs:(34,1)-(35,44)       7.0    0.2
aStarSet              Graph.Aop  Graph/Aop.hs:(80,3)-(95,8)        6.9    3.1
```

The top 5 entries are all for CSD cost! That's 79% of the time in total... I've fucked something up. 

I implemented a CSD algoritm to only return the number of non-zero bits, not converting to binary, converting to CSD, and then counting bits. Now runs in 0.77 seconds (down from 44! That's 1.75% of the original run-time).

Now implemented the optimised distance estimates, caching for all s and t pairs, because there's a lot of overlap in S and S'

Started optimising some of the AOp operations because they are called so much.
Including "caching" shift sets and not recomputing fOut.

Started looking at the heap output graphs and oh my! There was a space leak even going between calls to pickHcub... I've just searched and replaced `import Data.Map` for `import Data.Map.Strict` and it have solved everything. Now generating 100 sets of N=10 b=16 with 8MB of RAM... down from 300 MB.


Also noted that this runs faster on my laptop *without* threading...
