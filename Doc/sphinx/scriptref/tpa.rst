
.. _scriptref_tpa:

TPointArray Functions
=====================

Quicksort
---------

.. code-block:: pascal

    procedure Quicksort(var Arr : TIntegerArray);

Sorts a TIntegerArray using the Quicksort algorithm


tSwap
-----

.. code-block:: pascal

    procedure tSwap(var a, b: TPoint);

Swaps the values of a and b around


tpaSwap
-------

.. code-block:: pascal

    procedure tpaSwap(var a, b: TPointArray);

Swaps the values of a and b around


SwapE
-----

.. code-block:: pascal

    procedure SwapE(var a, b: Extended);

Swaps the values of a and b around


RAaSTPAEx
---------

.. code-block:: pascal

    procedure RAaSTPAEx(var a: TPointArray; const w, h: Integer);

Leaves one point per box with side lengths W and H to the TPA

RAaSTPA
-------

.. code-block:: pascal

    procedure RAaSTPA(var a: TPointArray; const Dist: Integer);

Leaves one point per box with the side length Dist


NearbyPointInArrayEx
--------------------

.. code-block:: pascal

    function NearbyPointInArrayEx(const P: TPoint; w, h:Integer;const  a: TPointArray): Boolean;

Returns true if the point P is near a point in the TPA a with the


NearbyPointInArray
------------------

.. code-block:: pascal

    function NearbyPointInArray(const P: TPoint; Dist:Integer;const  a: TPointArray): Boolean;

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

Sorts the TPA a from the TPoint From


SortATPAFrom
------------

.. code-block:: pascal

    procedure SortATPAFrom(var a: T2DPointArray; const From: TPoint);

Sorts the T2DPointArray a from the TPoint From


SortATPAFromFirstPoint
----------------------

.. code-block:: pascal

    procedure SortATPAFromFirstPoint(var a: T2DPointArray; const From: TPoint);


InvertTPA
---------

.. code-block:: pascal

    procedure InvertTPA(var a: TPointArray);

Reverses the TPA


InvertATPA
----------

.. code-block:: pascal

    procedure InvertATPA(var a: T2DPointArray);

Reverses the T2dPointArray


MiddleTPAEx
-----------

.. code-block:: pascal

    function MiddleTPAEx(const TPA: TPointArray; var x, y: Integer): Boolean;

Stores the middle point from the TPA in x and y 


MiddleTPA
---------

.. code-block:: pascal

    function MiddleTPA(const tpa: TPointArray): TPoint;

Returns the middle TPA in the result


SortATPASize
------------

.. code-block:: pascal

    procedure SortATPASize(var a: T2DPointArray; const BigFirst: Boolean);

Sorts the T2dPointArray from largest to smallest if BigFirst is true or smallest to largest if BigFirst is false


SortATPAFromSize
----------------

.. code-block:: pascal

    procedure SortATPAFromSize(var a: T2DPointArray; const Size: Integer; CloseFirst: Boolean);

Sorts the T2DPointArray from Size by the closest first if CloseFirst is true


InIntArrayEx
------------

.. code-block:: pascal

    function InIntArrayEx(const a: TIntegerArray; var Where: Integer; const Number: Integer): Boolean;

Returns true if Number was found in the TIntegerArray a and returns its location in Where


InIntArray
----------

.. code-block:: pascal

    function InIntArray(const a: TIntegerArray; Number: Integer): Boolean;

Returns true if Number is found in the TintegerArray a


ClearSameIntegers
-----------------

.. code-block:: pascal

    procedure ClearSameIntegers(var a: TIntegerArray);

Deletes the indexes in the TintegerArray a which are duplicated


ClearSameIntegersAndTPA
-----------------------

.. code-block:: pascal

    procedure ClearSameIntegersAndTPA(var a: TIntegerArray; var p: TPointArray);

Deletes the indexes in the TIntegerArray a and TPointArray p which are duplicated


SplitTPAEx
----------

.. code-block:: pascal

    function SplitTPAEx(const arr: TPointArray; w, h: Integer): T2DPointArray;

Splits the points with max X and Y distances W and H to their 


SplitTPA
--------

.. code-block:: pascal

    function SplitTPA(const arr: TPointArray; Dist: Integer): T2DPointArray;

Splits the points with max distance Dist to their own TPointArrays


FloodFillTPA
------------

.. code-block:: pascal

    function FloodFillTPA(const TPA : TPointArray) : T2DPointArray;


FilterPointsPie
---------------

.. code-block:: pascal

    procedure FilterPointsPie(var Points: TPointArray; const SD, ED, MinR, MaxR: Extended; Mx, My: Integer);

Removes the points that are in the TPointArray Points that are not within the the degrees SD (Strat Degrees) and 
    ED (End Degrees) and the radius' MinR (Min Radius) and MaxR (Max Radius) from the origin Mx and My


FilterPointsLine
----------------

.. code-block:: pascal

    procedure FilterPointsLine(var Points: TPointArray; Radial: Extended; Radius, MX, MY: Integer);

Returns the result in the TPointArray Points. Returns the points from the TPointArray Points that are on the line Radial from the center mx, my that is with the radius Radius


FilterPointsDist
----------------

.. code-block:: pascal

    procedure FilterPointsDist(var Points: TPointArray; const MinDist, MaxDist: Extended; Mx, My: Integer);

Removes the points from the TPointArray Points that are not within the radius MinDist (Min Distance) and MaxDist
    from the origin Mx and My


GetATPABounds
-------------

.. code-block:: pascal

    function GetATPABounds(const ATPA: T2DPointArray): TBox;

Returns the boundaries of the T2DPointArray ATPA as a TBox


GetTPABounds
------------

.. code-block:: pascal

    function GetTPABounds(const TPA: TPointArray): TBox;

Returns the boundaries of the TPointArray TPA as a TBox


FindTPAinTPA
------------

.. code-block:: pascal

    function FindTPAinTPA(const SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;

Looks for the TPoints from SearchTPA inside TotalTPA and stores the matches inside the TPointArray Matches


GetSamePointsATPA
-----------------

.. code-block:: pascal

    function GetSamePointsATPA(const  ATPA : T2DPointArray; var Matches : TPointArray) : boolean;

Finds duplicate Points inside the T2DPointArray ATPA and stores the results inside the TPointArray Matches


FindTextTPAinTPA
----------------

.. code-block:: pascal

    function FindTextTPAinTPA(Height : integer;const  SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;

Looks for the TPoints from SearchTPA inside TotalTPA with a maximum y distance of Height and stores the matches inside the TPointArray Matches


SortCircleWise
--------------

.. code-block:: pascal

    procedure SortCircleWise(var tpa: TPointArray; const cx, cy, StartDegree: Integer; SortUp, ClockWise: Boolean);

Sorts the TPointArray tpa from the point cx, cy if Sortup is true. Starting at StartDegree going clockwise if Clockwise is True 


LinearSort
----------

.. code-block:: pascal

    procedure LinearSort(var tpa: TPointArray; cx, cy, sd: Integer; SortUp: Boolean);

Sorts the TPointArray tpa from cx, cy if Sortup is true on the degree angle sd


RotatePoint
-----------

.. code-block:: pascal

    function RotatePoint(Const p: TPoint; angle, mx, my: Extended): TPoint;

Rotates the TPoint p around the center mx, my with the angle


ChangeDistPT
------------

.. code-block:: pascal

    function ChangeDistPT(const PT : TPoint; mx,my : integer; newdist : extended) : TPoint;

Returns a TPoint with the distance newdist from the point mx, my based on the position of the TPoint TP


ChangeDistTPA
-------------

.. code-block:: pascal

    function ChangeDistTPA(var TPA : TPointArray; mx,my : integer; newdist : extended) : boolean;

Returns the result in the TPointArray TPA with the distance newdist from mx, my based on the current position TPA


FindGapsTPA
-----------

.. code-block:: pascal

    function FindGapsTPA(const TPA: TPointArray; MinPixels: Integer): T2DPointArray;

Finds the possible gaps in the TPointArray TPA and results the gaps as a T2DPointArray. Considers as a gap if the gap length is >= MinPixels


RemoveDistTPointArray
---------------------

.. code-block:: pascal

    function RemoveDistTPointArray(x, y, dist: Integer;const  ThePoints: TPointArray; RemoveHigher: Boolean): TPointArray;

Finds the possible gaps in the TPointArray TPA and removes the gaps. Considers as a gap if the gap length is >= MinPixels


CombineTPA
----------

.. code-block:: pascal

    function CombineTPA(const Ar1, Ar2: TPointArray): TPointArray;

Attaches the TPointArray Ar2 onto the end of Ar1 and returns it as the result


ReArrangeandShortenArrayEx
--------------------------

.. code-block:: pascal

    function ReArrangeandShortenArrayEx(const a: TPointArray; w, h: Integer): TPointArray;

Results the TPointArray a with one point per box with side lengths W and H left


ReArrangeandShortenArray
------------------------

.. code-block:: pascal

    function ReArrangeandShortenArray(const a: TPointArray; Dist: Integer): TPointArray;

Results the TPointArray a with one point per box with side length Dist left


TPAtoATPAEx
-----------

.. code-block:: pascal

    function TPAtoATPAEx(const TPA: TPointArray; w, h: Integer): T2DPointArray;

Splits the TPA to boxes with sidelengths W and H and results them as a T2DPointArray


TPAtoATPA
---------

.. code-block:: pascal

    function TPAtoATPA(const TPA: TPointArray; Dist: Integer): T2DPointArray;

Splits the TPA to boxes with sidelength Dist and results them as a T2DPointArray


CombineIntArray
---------------

.. code-block:: pascal

    function CombineIntArray(const Ar1, Ar2: TIntegerArray): TIntegerArray;

Attaches the TIntegerArray Ar2 onto the end of Ar1 and returns it as the result


MergeATPA
---------

.. code-block:: pascal

    function MergeATPA(const ATPA : T2DPointArray)  : TPointArray;

Combines all the TPointArrays from the T2DPointArray ATPA into the result


AppendTPA
---------

.. code-block:: pascal

    procedure AppendTPA(var TPA: TPointArray; const ToAppend: TPointArray);

Attaches the TPointArray ToAppend onto the end of TPA


TPAFromBox
----------

.. code-block:: pascal

    function TPAFromBox(const Box : TBox) : TPointArray;

Create a TPointArray from the top left and the bottom right of the TBox Box


RotatePoints
------------

.. code-block:: pascal

    function RotatePoints(Const P: TPointArray; A, cx, cy: Extended): TPointArray ;

Rotates the TPointArray P around the center cx, cy with the angle a


FindTPAEdges
------------

.. code-block:: pascal

    function FindTPAEdges(const p: TPointArray): TPointArray;

Returns a TPointArray of the edge points of the TPointArray p


ClearTPAFromTPA
---------------

.. code-block:: pascal

    function ClearTPAFromTPA(const arP, ClearPoints: TPointArray): TPointArray;

Removes the points in TPointArray ClearPoints from arP


ReturnPointsNotInTPA
--------------------

.. code-block:: pascal

    function ReturnPointsNotInTPA(Const TotalTPA: TPointArray; const Box: TBox): TPointArray;

All the points from the TPointArray TotalTPA that are not in the TBox Box are returned in the TPointArray Res


PointInTPA
----------

.. code-block:: pascal

    function PointInTPA(p: TPoint;const  arP: TPointArray): Boolean;

Returns true if the TPoint p is found in the TPointArray arP


ClearDoubleTPA
--------------

.. code-block:: pascal

    procedure ClearDoubleTPA(var TPA: TPointArray);

Deletes duplicate TPAs int he TPointArray TPA


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

Reverses the TIntegerArray tI


SumIntegerArray
---------------

.. code-block:: pascal

    function SumIntegerArray(const Ints : TIntegerArray): Integer;

Retuns the sum of all the integers in the TIntegerArray Ints


AverageTIA
----------

.. code-block:: pascal

    function AverageTIA(const tI: TIntegerArray): Integer;

Gives an average of the sum of the integers in the TIntegerArray tI


AverageExtended
---------------

.. code-block:: pascal

    function AverageExtended(const tE: TExtendedArray): Extended;

Gives an average of the sum of the extendeds in the TExtendedArray tI


SplitTPAExWrap
--------------

.. code-block:: pascal

    procedure SplitTPAExWrap(const arr: TPointArray; w, h: Integer; var res : T2DPointArray);

Splits the points with max X and Y distances W and H to their and returns the result in the T2DPointArray Res


SplitTPAWrap
------------

.. code-block:: pascal

    procedure SplitTPAWrap(const arr: TPointArray; Dist: Integer; var res: T2DPointArray);

Splits the points with max distance Dist to their own TPointArrays and returns the result in the T2DPointArray Res


FindGapsTPAWrap
---------------

.. code-block:: pascal

    procedure FindGapsTPAWrap(const TPA: TPointArray; MinPixels: Integer; var Res : T2DPointArray);

Finds the possible gaps in the TPointArray TPA and the result is returned in the T2DPointArray Res. Considers as a gap if the gap length is >= MinPixels


RemoveDistTPointArrayWrap
-------------------------

.. code-block:: pascal

    procedure RemoveDistTPointArrayWrap(x, y, dist: Integer;const  ThePoints: TPointArray; RemoveHigher: Boolean; var Res :  TPointArray);

Finds the possible gaps in the TPointArray TPA and removes the gaps. Considers as a gap if the gap length is >= MinPixels and returns the result in the TPointArray Res


CombineTPAWrap
--------------

.. code-block:: pascal

    procedure CombineTPAWrap(const Ar1, Ar2: TPointArray; var Res :  TPointArray);

Attaches the TPointArray Ar2 onto the end of Ar1 and returns the result in the TPointArray Res


ReArrangeandShortenArrayExWrap
------------------------------

.. code-block:: pascal

    procedure ReArrangeandShortenArrayExWrap(const a: TPointArray; w, h: Integer; var Res :  TPointArray);

Results the TPointArray a with one point per box with side lengths W and H left and puts the result in Res


ReArrangeandShortenArrayWrap
----------------------------

.. code-block:: pascal

    procedure ReArrangeandShortenArrayWrap(const a: TPointArray; Dist: Integer; var Res :  TPointArray);

Results the TPointArray a with one point per box with side length Dist left and puts the result in Res


TPAtoATPAExWrap
---------------

.. code-block:: pascal

    procedure TPAtoATPAExWrap(const TPA: TPointArray; w, h: Integer; var Res :  T2DPointArray);

Splits the TPA to boxes with sidelengths W and H and results them as a T2DPointArray in Res


TPAtoATPAWrap
-------------

.. code-block:: pascal

    procedure TPAtoATPAWrap(const TPA: TPointArray; Dist: Integer; var Res :  T2DPointArray);

Splits the TPA to boxes with sidelength Dist and results them as a T2DPointArray in Res


CombineIntArrayWrap
-------------------

.. code-block:: pascal

    procedure CombineIntArrayWrap(const Ar1, Ar2: TIntegerArray; var Res :  TIntegerArray);

Attaches the TIntegerArray Ar2 onto the end of Ar1 and returns it in the TIntegerArray Res


ReturnPointsNotInTPAWrap
------------------------

.. code-block:: pascal

    procedure ReturnPointsNotInTPAWrap(Const TotalTPA: TPointArray; const Box: TBox; var Res :  TPointArray);

All the points from the TPointArray TotalTPA that are not in the TBox Box are returned in the TPointArray Res


MergeATPAWrap
-------------

.. code-block:: pascal

    procedure MergeATPAWrap(const ATPA : T2DPointArray; var Res: TPointArray);

Combines all the TPointArrays from the T2DPointArray ATPA into the TPointArray Res


TPAFromBoxWrap
--------------

.. code-block:: pascal

    procedure TPAFromBoxWrap(const Box : TBox; var Res : TPointArray);

Create a TPointArray from the top left and the bottom right of the TBox Box and returns the result in Res


RotatePointsWrap
----------------

.. code-block:: pascal

    procedure RotatePointsWrap(Const P: TPointArray; A, cx, cy: Extended; var Res :  TPointArray);

Rotates the TPointArray P around the center cx, cy with the angle a and returns the result in Res


FindTPAEdgesWrap
----------------

.. code-block:: pascal

    procedure FindTPAEdgesWrap(const p: TPointArray; var Res :  TPointArray);

Returns a TPointArray of the edge points of the TPointArray p and returns the result in the TPointArray Res


ClearTPAFromTPAWrap
-------------------

.. code-block:: pascal

    procedure ClearTPAFromTPAWrap(const arP, ClearPoints: TPointArray;  var Res :  TPointArray);

Removes the points in TPointArray ClearPoints from arP and returns the results in Res


SameTPA
-------

.. code-block:: pascal

    function SameTPA(const aTPA, bTPA: TPointArray): Boolean;

Returns true if the TPointArray aTPA is the same as bTPA 


TPAInATPA
---------

.. code-block:: pascal

    function TPAInATPA(const TPA: TPointArray;const  InATPA: T2DPointArray; var Index: LongInt): Boolean;

Returns true if the TPointArray TPA is found in the T2DPointArray InATPA and stores the index in Index


OffsetTPA
---------

.. code-block:: pascal

    procedure OffsetTPA(var TPA : TPointArray; const Offset : TPoint);

Offsets all the TPAs int the TPointArray TPA but the TPoint Offset


OffsetATPA
----------

.. code-block:: pascal

    procedure OffsetATPA(var ATPA : T2DPointArray; const Offset : TPoint);

Offsets all the TPAs int the T2DPointArray ATPA but the TPoint Offset


CopyTPA
-------

.. code-block:: pascal

    function CopyTPA(const TPA : TPointArray) : TPointArray;

Returns the TPointArray TPA


CopyATPA
--------

.. code-block:: pascal

    function CopyATPA(const ATPA : T2DPointArray) : T2DPointArray;

Returns the T2DPointArray ATPA

