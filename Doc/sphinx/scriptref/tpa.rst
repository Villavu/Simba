
.. _scriptref_tpa:

TPointArray Functions
=====================

Quicksort
---------

.. code-block:: pascal

    procedure Quicksort(var Arr : TIntegerArray);


tSwap
-----

.. code-block:: pascal

    procedure tSwap(var a, b: TPoint);


tpaSwap
-------

.. code-block:: pascal

    procedure tpaSwap(var a, b: TPointArray);


SwapE
-----

.. code-block:: pascal

    procedure SwapE(var a, b: Extended);


RAaSTPAEx
---------

.. code-block:: pascal

    procedure RAaSTPAEx(var a: TPointArray; const w, h: Integer);


RAaSTPA
-------

.. code-block:: pascal

    procedure RAaSTPA(var a: TPointArray; const Dist: Integer);


NearbyPointInArrayEx
--------------------

.. code-block:: pascal

    function NearbyPointInArrayEx(const P: TPoint; w, h:Integer;const  a: TPointArray): Boolean;


NearbyPointInArray
------------------

.. code-block:: pascal

    function NearbyPointInArray(const P: TPoint; Dist:Integer;const  a: TPointArray): Boolean;


QuickTPASort
------------

.. code-block:: pascal

    procedure QuickTPASort(var A: TIntegerArray; var B: TPointArray; iLo, iHi: Integer; SortUp: Boolean);


QuickATPASort
-------------

.. code-block:: pascal

    procedure QuickATPASort(var A: TIntegerArray; var B: T2DPointArray; iLo, iHi: Integer; SortUp: Boolean);


SortTPAFrom
-----------

.. code-block:: pascal

    procedure SortTPAFrom(var a: TPointArray; const From: TPoint);


SortATPAFrom
------------

.. code-block:: pascal

    procedure SortATPAFrom(var a: T2DPointArray; const From: TPoint);


SortATPAFromFirstPoint
----------------------

.. code-block:: pascal

    procedure SortATPAFromFirstPoint(var a: T2DPointArray; const From: TPoint);


InvertTPA
---------

.. code-block:: pascal

    procedure InvertTPA(var a: TPointArray);


InvertATPA
----------

.. code-block:: pascal

    procedure InvertATPA(var a: T2DPointArray);


MiddleTPAEx
-----------

.. code-block:: pascal

    function MiddleTPAEx(const TPA: TPointArray; var x, y: Integer): Boolean;


MiddleTPA
---------

.. code-block:: pascal

    function MiddleTPA(const tpa: TPointArray): TPoint;


SortATPASize
------------

.. code-block:: pascal

    procedure SortATPASize(var a: T2DPointArray; const BigFirst: Boolean);


SortATPAFromSize
----------------

.. code-block:: pascal

    procedure SortATPAFromSize(var a: T2DPointArray; const Size: Integer; CloseFirst: Boolean);


InIntArrayEx
------------

.. code-block:: pascal

    function InIntArrayEx(const a: TIntegerArray; var Where: Integer; const Number: Integer): Boolean;


InIntArray
----------

.. code-block:: pascal

    function InIntArray(const a: TIntegerArray; Number: Integer): Boolean;


ClearSameIntegers
-----------------

.. code-block:: pascal

    procedure ClearSameIntegers(var a: TIntegerArray);


ClearSameIntegersAndTPA
-----------------------

.. code-block:: pascal

    procedure ClearSameIntegersAndTPA(var a: TIntegerArray; var p: TPointArray);


SplitTPAEx
----------

.. code-block:: pascal

    function SplitTPAEx(const arr: TPointArray; w, h: Integer): T2DPointArray;


SplitTPA
--------

.. code-block:: pascal

    function SplitTPA(const arr: TPointArray; Dist: Integer): T2DPointArray;


FloodFillTPA
------------

.. code-block:: pascal

    function FloodFillTPA(const TPA : TPointArray) : T2DPointArray;


FilterPointsPie
---------------

.. code-block:: pascal

    procedure FilterPointsPie(var Points: TPointArray; const SD, ED, MinR, MaxR: Extended; Mx, My: Integer);


FilterPointsLine
----------------

.. code-block:: pascal

    procedure FilterPointsLine(var Points: TPointArray; Radial: Extended; Radius, MX, MY: Integer);


FilterPointsDist
----------------

.. code-block:: pascal

    procedure FilterPointsDist(var Points: TPointArray; const MinDist, MaxDist: Extended; Mx, My: Integer);


GetATPABounds
-------------

.. code-block:: pascal

    function GetATPABounds(const ATPA: T2DPointArray): TBox;


GetTPABounds
------------

.. code-block:: pascal

    function GetTPABounds(const TPA: TPointArray): TBox;


FindTPAinTPA
------------

.. code-block:: pascal

    function FindTPAinTPA(const SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;


GetSamePointsATPA
-----------------

.. code-block:: pascal

    function GetSamePointsATPA(const  ATPA : T2DPointArray; var Matches : TPointArray) : boolean;


FindTextTPAinTPA
----------------

.. code-block:: pascal

    function FindTextTPAinTPA(Height : integer;const  SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;


SortCircleWise
--------------

.. code-block:: pascal

    procedure SortCircleWise(var tpa: TPointArray; const cx, cy, StartDegree: Integer; SortUp, ClockWise: Boolean);


LinearSort
----------

.. code-block:: pascal

    procedure LinearSort(var tpa: TPointArray; cx, cy, sd: Integer; SortUp: Boolean);


RotatePoint
-----------

.. code-block:: pascal

    function RotatePoint(Const p: TPoint; angle, mx, my: Extended): TPoint;


ChangeDistPT
------------

.. code-block:: pascal

    function ChangeDistPT(const PT : TPoint; mx,my : integer; newdist : extended) : TPoint;


ChangeDistTPA
-------------

.. code-block:: pascal

    function ChangeDistTPA(var TPA : TPointArray; mx,my : integer; newdist : extended) : boolean;


FindGapsTPA
-----------

.. code-block:: pascal

    function FindGapsTPA(const TPA: TPointArray; MinPixels: Integer): T2DPointArray;


RemoveDistTPointArray
---------------------

.. code-block:: pascal

    function RemoveDistTPointArray(x, y, dist: Integer;const  ThePoints: TPointArray; RemoveHigher: Boolean): TPointArray;


CombineTPA
----------

.. code-block:: pascal

    function CombineTPA(const Ar1, Ar2: TPointArray): TPointArray;


ReArrangeandShortenArrayEx
--------------------------

.. code-block:: pascal

    function ReArrangeandShortenArrayEx(const a: TPointArray; w, h: Integer): TPointArray;


ReArrangeandShortenArray
------------------------

.. code-block:: pascal

    function ReArrangeandShortenArray(const a: TPointArray; Dist: Integer): TPointArray;


TPAtoATPAEx
-----------

.. code-block:: pascal

    function TPAtoATPAEx(const TPA: TPointArray; w, h: Integer): T2DPointArray;


TPAtoATPA
---------

.. code-block:: pascal

    function TPAtoATPA(const TPA: TPointArray; Dist: Integer): T2DPointArray;


CombineIntArray
---------------

.. code-block:: pascal

    function CombineIntArray(const Ar1, Ar2: TIntegerArray): TIntegerArray;


MergeATPA
---------

.. code-block:: pascal

    function MergeATPA(const ATPA : T2DPointArray)  : TPointArray;


AppendTPA
---------

.. code-block:: pascal

    procedure AppendTPA(var TPA: TPointArray; const ToAppend: TPointArray);


TPAFromBox
----------

.. code-block:: pascal

    function TPAFromBox(const Box : TBox) : TPointArray;


RotatePoints
------------

.. code-block:: pascal

    function RotatePoints(Const P: TPointArray; A, cx, cy: Extended): TPointArray ;


FindTPAEdges
------------

.. code-block:: pascal

    function FindTPAEdges(const p: TPointArray): TPointArray;


ClearTPAFromTPA
---------------

.. code-block:: pascal

    function ClearTPAFromTPA(const arP, ClearPoints: TPointArray): TPointArray;


ReturnPointsNotInTPA
--------------------

.. code-block:: pascal

    function ReturnPointsNotInTPA(Const TotalTPA: TPointArray; const Box: TBox): TPointArray;


PointInTPA
----------

.. code-block:: pascal

    function PointInTPA(p: TPoint;const  arP: TPointArray): Boolean;


ClearDoubleTPA
--------------

.. code-block:: pascal

    procedure ClearDoubleTPA(var TPA: TPointArray);


TPACountSort
------------

.. code-block:: pascal

    procedure TPACountSort(Var TPA: TPointArray;const max: TPoint;Const SortOnX : Boolean);


TPACountSortBase
----------------

.. code-block:: pascal

    procedure TPACountSortBase(Var TPA: TPointArray;const maxx, base: TPoint; const SortOnX : Boolean);


InvertTIA
---------

.. code-block:: pascal

    procedure InvertTIA(var tI: TIntegerArray);


SumIntegerArray
---------------

.. code-block:: pascal

    function SumIntegerArray(const Ints : TIntegerArray): Integer;


AverageTIA
----------

.. code-block:: pascal

    function AverageTIA(const tI: TIntegerArray): Integer;


AverageExtended
---------------

.. code-block:: pascal

    function AverageExtended(const tE: TExtendedArray): Extended;


SplitTPAExWrap
--------------

.. code-block:: pascal

    procedure SplitTPAExWrap(const arr: TPointArray; w, h: Integer; var res : T2DPointArray);


SplitTPAWrap
------------

.. code-block:: pascal

    procedure SplitTPAWrap(const arr: TPointArray; Dist: Integer; var res: T2DPointArray);


FindGapsTPAWrap
---------------

.. code-block:: pascal

    procedure FindGapsTPAWrap(const TPA: TPointArray; MinPixels: Integer; var Res : T2DPointArray);


RemoveDistTPointArrayWrap
-------------------------

.. code-block:: pascal

    procedure RemoveDistTPointArrayWrap(x, y, dist: Integer;const  ThePoints: TPointArray; RemoveHigher: Boolean; var Res :  TPointArray);


CombineTPAWrap
--------------

.. code-block:: pascal

    procedure CombineTPAWrap(const Ar1, Ar2: TPointArray; var Res :  TPointArray);


ReArrangeandShortenArrayExWrap
------------------------------

.. code-block:: pascal

    procedure ReArrangeandShortenArrayExWrap(const a: TPointArray; w, h: Integer; var Res :  TPointArray);


ReArrangeandShortenArrayWrap
----------------------------

.. code-block:: pascal

    procedure ReArrangeandShortenArrayWrap(const a: TPointArray; Dist: Integer; var Res :  TPointArray);


TPAtoATPAExWrap
---------------

.. code-block:: pascal

    procedure TPAtoATPAExWrap(const TPA: TPointArray; w, h: Integer; var Res :  T2DPointArray);


TPAtoATPAWrap
-------------

.. code-block:: pascal

    procedure TPAtoATPAWrap(const TPA: TPointArray; Dist: Integer; var Res :  T2DPointArray);


CombineIntArrayWrap
-------------------

.. code-block:: pascal

    procedure CombineIntArrayWrap(const Ar1, Ar2: TIntegerArray; var Res :  TIntegerArray);


ReturnPointsNotInTPAWrap
------------------------

.. code-block:: pascal

    procedure ReturnPointsNotInTPAWrap(Const TotalTPA: TPointArray; const Box: TBox; var Res :  TPointArray);


MergeATPAWrap
-------------

.. code-block:: pascal

    procedure MergeATPAWrap(const ATPA : T2DPointArray; var Res: TPointArray);


TPAFromBoxWrap
--------------

.. code-block:: pascal

    procedure TPAFromBoxWrap(const Box : TBox; var Res : TPointArray);


RotatePointsWrap
----------------

.. code-block:: pascal

    procedure RotatePointsWrap(Const P: TPointArray; A, cx, cy: Extended; var Res :  TPointArray);


FindTPAEdgesWrap
----------------

.. code-block:: pascal

    procedure FindTPAEdgesWrap(const p: TPointArray; var Res :  TPointArray);


ClearTPAFromTPAWrap
-------------------

.. code-block:: pascal

    procedure ClearTPAFromTPAWrap(const arP, ClearPoints: TPointArray;  var Res :  TPointArray);


SameTPA
-------

.. code-block:: pascal

    function SameTPA(const aTPA, bTPA: TPointArray): Boolean;


TPAInATPA
---------

.. code-block:: pascal

    function TPAInATPA(const TPA: TPointArray;const  InATPA: T2DPointArray; var Index: LongInt): Boolean;


OffsetTPA
---------

.. code-block:: pascal

    procedure OffsetTPA(var TPA : TPointArray; const Offset : TPoint);


OffsetATPA
----------

.. code-block:: pascal

    procedure OffsetATPA(var ATPA : T2DPointArray; const Offset : TPoint);


CopyTPA
-------

.. code-block:: pascal

    function CopyTPA(const TPA : TPointArray) : TPointArray;


CopyATPA
--------

.. code-block:: pascal

    function CopyATPA(const ATPA : T2DPointArray) : T2DPointArray;


