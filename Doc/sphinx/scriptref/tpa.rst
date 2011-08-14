
.. _scriptref_tpa:

TPointArray Functions
=====================

Quicksort
---------

.. code-block:: pascal

    procedure Quicksort(var Arr : TIntegerArray);

.. note::

    Sorts a TIntegerArray using the Quicksort algorithm


tSwap
-----

.. code-block:: pascal

    procedure tSwap(var a, b: TPoint);

.. note::

    Swaps the values of a and b around


tpaSwap
-------

.. code-block:: pascal

    procedure tpaSwap(var a, b: TPointArray);

.. note::

    Swaps the values of a and b around


SwapE
-----

.. code-block:: pascal

    procedure SwapE(var a, b: Extended);

.. note::

    Swaps the values of a and b around


RAaSTPAEx
---------

.. code-block:: pascal

    procedure RAaSTPAEx(var a: TPointArray; const w, h: Integer);

.. note::

    Leaves one point per box with side lengths W and H to the TPA
    

RAaSTPA
-------

.. code-block:: pascal

    procedure RAaSTPA(var a: TPointArray; const Dist: Integer);

.. note::

    Leaves one point per box with the side length Dist


NearbyPointInArrayEx
--------------------

.. code-block:: pascal

    function NearbyPointInArrayEx(const P: TPoint; w, h:Integer;const  a: TPointArray): Boolean;

.. note::

    Returns true if the point P is near a point in the TPA a with the


NearbyPointInArray
------------------

.. code-block:: pascal

    function NearbyPointInArray(const P: TPoint; Dist:Integer;const  a: TPointArray): Boolean;

.. note::

    Returns true if the point P is near a point in the TPA a with the


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

.. note::

    Sorts the TPA a from the TPoint From


SortATPAFrom
------------

.. code-block:: pascal

    procedure SortATPAFrom(var a: T2DPointArray; const From: TPoint);

.. note::

    Sorts the T2DPointArray a from the TPoint From


SortATPAFromFirstPoint
----------------------

.. code-block:: pascal

    procedure SortATPAFromFirstPoint(var a: T2DPointArray; const From: TPoint);


InvertTPA
---------

.. code-block:: pascal

    procedure InvertTPA(var a: TPointArray);

.. note::

    Reverses the TPA


InvertATPA
----------

.. code-block:: pascal

    procedure InvertATPA(var a: T2DPointArray);

.. note::

    Reverses the T2dPointArray


MiddleTPAEx
-----------

.. code-block:: pascal

    function MiddleTPAEx(const TPA: TPointArray; var x, y: Integer): Boolean;

.. note::

    Stores the middle point from the TPA in x and y 


MiddleTPA
---------

.. code-block:: pascal

    function MiddleTPA(const tpa: TPointArray): TPoint;

.. note::

    Returns the middle TPA in the result


SortATPASize
------------

.. code-block:: pascal

    procedure SortATPASize(var a: T2DPointArray; const BigFirst: Boolean);

.. note::

    Sorts the T2dPointArray from largest to smallest if BigFirst is true or smallest to largest if BigFirst is false


SortATPAFromSize
----------------

.. code-block:: pascal

    procedure SortATPAFromSize(var a: T2DPointArray; const Size: Integer; CloseFirst: Boolean);

.. note::

    Sorts the T2DPointArray from Size by the closest first if CloseFirst is true


InIntArrayEx
------------

.. code-block:: pascal

    function InIntArrayEx(const a: TIntegerArray; var Where: Integer; const Number: Integer): Boolean;

.. note::

    Returns true if Number was found in the TIntegerArray a and returns its location in Where


InIntArray
----------

.. code-block:: pascal

    function InIntArray(const a: TIntegerArray; Number: Integer): Boolean;

.. note::

    Returns true if Number is found in the TintegerArray a


ClearSameIntegers
-----------------

.. code-block:: pascal

    procedure ClearSameIntegers(var a: TIntegerArray);

.. note::

    Deletes the indexes in the TintegerArray a which are duplicated


ClearSameIntegersAndTPA
-----------------------

.. code-block:: pascal

    procedure ClearSameIntegersAndTPA(var a: TIntegerArray; var p: TPointArray);

.. note::

    Deletes the indexes in the TIntegerArray a and TPointArray p which are duplicated


SplitTPAEx
----------

.. code-block:: pascal

    function SplitTPAEx(const arr: TPointArray; w, h: Integer): T2DPointArray;

.. note::

    Splits the points with max X and Y distances W and H to their 


SplitTPA
--------

.. code-block:: pascal

    function SplitTPA(const arr: TPointArray; Dist: Integer): T2DPointArray;

.. note::

    Splits the points with max distance Dist to their own TPointArrays


FloodFillTPA
------------

.. code-block:: pascal

    function FloodFillTPA(const TPA : TPointArray) : T2DPointArray;


FilterPointsPie
---------------

.. code-block:: pascal

    procedure FilterPointsPie(var Points: TPointArray; const SD, ED, MinR, MaxR: Extended; Mx, My: Integer);

.. note::

    Removes the points that are in the TPointArray Points that are not within the the degrees SD (Strat Degrees) and 
    ED (End Degrees) and the radius' MinR (Min Radius) and MaxR (Max Radius) from the origin Mx and My


FilterPointsLine
----------------

.. code-block:: pascal

    procedure FilterPointsLine(var Points: TPointArray; Radial: Extended; Radius, MX, MY: Integer);


FilterPointsDist
----------------

.. code-block:: pascal

    procedure FilterPointsDist(var Points: TPointArray; const MinDist, MaxDist: Extended; Mx, My: Integer);

.. note::

    Removes the points from the TPointArray Points that are not within the radius MinDist (Min Distance) and MaxDist
    from the origin Mx and My


GetATPABounds
-------------

.. code-block:: pascal

    function GetATPABounds(const ATPA: T2DPointArray): TBox;

.. note::

    Returns the boundaries of the T2DPointArray ATPA as a TBox


GetTPABounds
------------

.. code-block:: pascal

    function GetTPABounds(const TPA: TPointArray): TBox;

.. note::

    Returns the boundaries of the TPointArray TPA as a TBox


FindTPAinTPA
------------

.. code-block:: pascal

    function FindTPAinTPA(const SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;

.. note::

    Looks for the TPoints from SearchTPA inside TotalTPA and stores the matches inside the TPointArray Matches


GetSamePointsATPA
-----------------

.. code-block:: pascal

    function GetSamePointsATPA(const  ATPA : T2DPointArray; var Matches : TPointArray) : boolean;

.. note::

    Finds duplicate Points inside the T2DPointArray ATPA and stores the results inside the TPointArray Matches


FindTextTPAinTPA
----------------

.. code-block:: pascal

    function FindTextTPAinTPA(Height : integer;const  SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;

.. note::

    Looks for the TPoints from SearchTPA inside TotalTPA with a maximum y distance of Height and stores the matches inside the TPointArray Matches


SortCircleWise
--------------

.. code-block:: pascal

    procedure SortCircleWise(var tpa: TPointArray; const cx, cy, StartDegree: Integer; SortUp, ClockWise: Boolean);

.. note::

    Sorts the TPointArray tpa from the point cx, cy if Sortup is true. Starting at StartDegree going clockwise if Clockwise is True 


LinearSort
----------

.. code-block:: pascal

    procedure LinearSort(var tpa: TPointArray; cx, cy, sd: Integer; SortUp: Boolean);

.. note::

    Sorts the TPointArray tpa from cx, cy if Sortup is true on the degree angle sd


RotatePoint
-----------

.. code-block:: pascal

    function RotatePoint(Const p: TPoint; angle, mx, my: Extended): TPoint;

.. note::

    Rotates the TPoint p around the center mx, my with the angle


ChangeDistPT
------------

.. code-block:: pascal

    function ChangeDistPT(const PT : TPoint; mx,my : integer; newdist : extended) : TPoint;

.. note::

    Returns a TPoint with the distance newdist from the point mx, my based on the position of the TPoint TP


ChangeDistTPA
-------------

.. code-block:: pascal

    function ChangeDistTPA(var TPA : TPointArray; mx,my : integer; newdist : extended) : boolean;


FindGapsTPA
-----------

.. code-block:: pascal

    function FindGapsTPA(const TPA: TPointArray; MinPixels: Integer): T2DPointArray;

.. note::

    Finds the possible gaps in the TPointArray TPA and results the gaps as a T2DPointArray. Considers as a gap if the gap length is >= MinPixels


RemoveDistTPointArray
---------------------

.. code-block:: pascal

    function RemoveDistTPointArray(x, y, dist: Integer;const  ThePoints: TPointArray; RemoveHigher: Boolean): TPointArray;

.. note::

    Finds the possible gaps in the TPointArray TPA and removes the gaps. Considers as a gap if the gap length is >= MinPixels


CombineTPA
----------

.. code-block:: pascal

    function CombineTPA(const Ar1, Ar2: TPointArray): TPointArray;

.. note::

    Attaches the TPointArray Ar2 onto the end of Ar1 and returns it as the result


ReArrangeandShortenArrayEx
--------------------------

.. code-block:: pascal

    function ReArrangeandShortenArrayEx(const a: TPointArray; w, h: Integer): TPointArray;

.. note::

    Results the TPointArray a with one point per box with side lengths W and H left


ReArrangeandShortenArray
------------------------

.. code-block:: pascal

    function ReArrangeandShortenArray(const a: TPointArray; Dist: Integer): TPointArray;

.. note::
   
    Results the TPointArray a with one point per box with side length Dist left


TPAtoATPAEx
-----------

.. code-block:: pascal

    function TPAtoATPAEx(const TPA: TPointArray; w, h: Integer): T2DPointArray;

.. note::

    Splits the TPA to boxes with sidelengths W and H and results them as a T2DPointArray


TPAtoATPA
---------

.. code-block:: pascal

    function TPAtoATPA(const TPA: TPointArray; Dist: Integer): T2DPointArray;

.. note::

    Splits the TPA to boxes with sidelength Dist and results them as a T2DPointArray


CombineIntArray
---------------

.. code-block:: pascal

    function CombineIntArray(const Ar1, Ar2: TIntegerArray): TIntegerArray;

.. note::

    Attaches the TIntegerArray Ar2 onto the end of Ar1 and returns it as the result


MergeATPA
---------

.. code-block:: pascal

    function MergeATPA(const ATPA : T2DPointArray)  : TPointArray;

.. note::

    Combines all the TPointArrays from the T2DPointArray ATPA into the result


AppendTPA
---------

.. code-block:: pascal

    procedure AppendTPA(var TPA: TPointArray; const ToAppend: TPointArray);

.. note::

    Attaches the TPointArray ToAppend onto the end of TPA


TPAFromBox
----------

.. code-block:: pascal

    function TPAFromBox(const Box : TBox) : TPointArray;

.. note::

    Create a TPointArray from the top left and the bottom right of the TBox Box


RotatePoints
------------

.. code-block:: pascal

    function RotatePoints(Const P: TPointArray; A, cx, cy: Extended): TPointArray ;

.. note::

    Rotates the TPointArray P around the center cx, cy with the angle a


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


