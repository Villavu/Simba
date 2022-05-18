pushSection('Arrays & Algorithms');

//------- array.Difference(array)
addGlobalFunc('function TByteArray.Difference(Other: TByteArray): TByteArray', @_Lape_UInt8_Difference);
addGlobalFunc('function TIntegerArray.Difference(Other: TIntegerArray): TIntegerArray', @_Lape_Int32_Difference);
addGlobalFunc('function TInt64Array.Difference(Other: TInt64Array): TInt64Array', @_Lape_Int64_Difference);
addGlobalFunc('function TPointArray.Difference(Other: TPointArray): TPointArray', @_Lape_Point_Difference);
addGlobalFunc('function TBoxArray.Difference(Other: TBoxArray): TBoxArray', @_Lape_Box_Difference);

//------- array.SymmetricDifference(array)
addGlobalFunc('function TByteArray.SymmetricDifference(Other: TByteArray): TByteArray', @_Lape_UInt8_SymmetricDifference);
addGlobalFunc('function TIntegerArray.SymmetricDifference(Other: TIntegerArray): TIntegerArray', @_Lape_Int32_SymmetricDifference);
addGlobalFunc('function TInt64Array.SymmetricDifference(Other: TInt64Array): TInt64Array', @_Lape_Int64_SymmetricDifference);
addGlobalFunc('function TPointArray.SymmetricDifference(Other: TPointArray): TPointArray', @_Lape_Point_SymmetricDifference);
addGlobalFunc('function TBoxArray.SymmetricDifference(Other: TBoxArray): TBoxArray', @_Lape_Box_SymmetricDifference);

//------- array.Intersection(array)
addGlobalFunc('function TByteArray.Intersection(Other: TByteArray): TByteArray', @_Lape_UInt8_Intersection);
addGlobalFunc('function TIntegerArray.Intersection(Other: TIntegerArray): TIntegerArray', @_Lape_Int32_Intersection);
addGlobalFunc('function TInt64Array.Intersection(Other: TInt64Array): TInt64Array', @_Lape_Int64_Intersection);
addGlobalFunc('function TPointArray.Intersection(Other: TPointArray): TPointArray', @_Lape_Point_Intersection);
addGlobalFunc('function TBoxArray.Intersection(Other: TBoxArray): TBoxArray', @_Lape_Box_Intersection);

addGlobalFunc('function NearbyPointInArrayEx(P: TPoint; W, H: Integer; const TPA: TPointArray): Boolean', @_LapeNearbyPointInArrayEx);
addGlobalFunc('function NearbyPointInArray(P: TPoint; Dist: Integer; const TPA: TPointArray): Boolean', @_LapeNearbyPointInArray);
addGlobalFunc('procedure SortTPAByX(var TPA: TPointArray; LowToHi: Boolean)', @_LapeSortTPAByX);
addGlobalFunc('procedure SortTPAByY(var TPA: TPointArray; LowToHi: Boolean)', @_LapeSortTPAByY);
addGlobalFunc('function FindTPARows(const TPA: TPointArray): T2DPointArray', @_LapeFindTPARows);
addGlobalFunc('function FindTPAColumns(const TPA: TPointArray): T2DPointArray', @_LapeFindTPAColumns);
addGlobalFunc('procedure SortTPAFrom(var TPA: TPointArray; From: TPoint)', @_LapeSortTPAFrom);
addGlobalFunc('procedure SortATPAFromFirstPoint(var ATPA: T2DPointArray; From: TPoint)', @_LapeSortATPAFromFirstPoint);
addGlobalFunc('procedure SortATPAFromMidPoint(var ATPA: T2DPointArray; From: TPoint)', @_LapeSortATPAFromMidPoint);
addGlobalFunc('procedure SortATPAFromFirstPointX(var ATPA: T2DPointArray; From: TPoint)', @_LapeSortATPAFromFirstPointX);
addGlobalFunc('procedure SortATPAFromFirstPointY(var ATPA: T2DPointArray; From: TPoint)', @_LapeSortATPAFromFirstPointY);
addGlobalFunc('function MiddleTPAEx(const TPA: TPointArray; var X, Y: Integer): Boolean', @_LapeMiddleTPAEx);
addGlobalFunc('function MiddleTPA(const TPA: TPointArray): TPoint', @_LapeMiddleTPA);
addGlobalFunc('procedure MedianTPAEx(const TPA: TPointArray; out X, Y: Integer)', @_LapeMedianTPAEx);
addGlobalFunc('function MedianTPA(const TPA: TPointArray): TPoint', @_LapeMedianTPA);
addGlobalFunc('procedure SortATPASize(var ATPA: T2DPointArray; BigFirst: Boolean)', @_LapeSortATPASize);
addGlobalFunc('procedure SortATPAFromSize(var ATPA: T2DPointArray; Size: Integer; CloseFirst: Boolean)', @_LapeSortATPAFromSize);
addGlobalFunc('procedure FilterTPAsBetween(var ATPA: T2DPointArray; MinLength, MaxLength: Integer)', @_LapeFilterTPAsBetween);
addGlobalFunc('function SplitTPAEx(const Arr: TPointArray; W, H: Integer): T2DPointArray', @_LapeSplitTPAEx);
addGlobalFunc('function SplitTPA(const Arr: TPointArray; Dist: Integer): T2DPointArray', @_LapeSplitTPA);
addGlobalFunc('function ClusterTPAEx(const TPA: TPointArray; Width, Height: Integer): T2DPointArray', @_LapeClusterTPAEx);
addGlobalFunc('function ClusterTPA(const TPA: TPointArray; Dist: Integer): T2DPointArray', @_LapeClusterTPA);
addGlobalFunc('procedure FilterPointsPie(var Points: TPointArray; SD, ED, MinR, MaxR: Extended; Mx, My: Integer)', @_LapeFilterPointsPie);
addGlobalFunc('procedure FilterPointsDist(var Points: TPointArray; MinDist, MaxDist: Extended; Mx, My: Integer)', @_LapeFilterPointsDist);
addGlobalFunc('procedure FilterPointsLine(var Points: TPointArray; Radial: Extended; Radius, MX, MY: Integer)', @_LapeFilterPointsLine);
addGlobalFunc('procedure FilterPointsBox(var points: TPointArray; X1, Y1, X2, Y2: Integer)', @_LapeFilterPointsBox);
addGlobalFunc('function GetATPABounds(const ATPA: T2DPointArray): TBox', @_LapeGetATPABounds);
addGlobalFunc('function GetTPABounds(const TPA: TPointArray): TBox', @_LapeGetTPABounds);
addGlobalFunc('function GetSamePointsATPA(const ATPA: T2DPointArray; var Matches: TPointArray): Boolean', @_LapeGetSamePointsATPA);
addGlobalFunc('function TPAtoATPAEx(const TPA: TPointArray; W, H: Integer): T2DPointArray', @_LapeTPAtoATPAEx);
addGlobalFunc('function TPAtoATPA(const TPA: TPointArray; Dist: Integer): T2DPointArray', @_LapeTPAtoATPA);
addGlobalFunc('function MergeATPA(const ATPA: T2DPointArray): TPointArray', @_LapeMergeATPA);
addGlobalFunc('function TPAFromLine(X1, Y1, X2, Y2: Integer; Thickness: Integer = 1): TPointArray; overload', @_LapeTPAFromLine);
addGlobalFunc('function TPAFromLine(P1, P2: TPoint; Thickness: Integer = 1): TPointArray; overload', @_LapeTPAFromLineEx);
addGlobalFunc('function EdgeFromBox(const Box: TBox): TPointArray', @_LapeEdgeFromBox);
addGlobalFunc('function TPAFromBox(const Box: TBox): TPointArray', @_LapeTPAFromBox);
addGlobalFunc('function TPAFromEllipse(CX, CY, XRadius, YRadius: Integer) : TPointArray', @_LapeTPAFromEllipse);
addGlobalFunc('function TPAFromCircle(CX, CY, Radius: Integer): TPointArray', @_LapeTPAFromCircle);
addGlobalFunc('function TPAFromPolygon(const Shape: TPointArray) : TPointArray', @_LapeTPAFromPolygon);
addGlobalFunc('procedure FillEllipse(var TPA: TPointArray)', @_LapeFillEllipse);
addGlobalFunc('function RotatePoint(const p: TPoint; angle, mx, my: Extended): TPoint', @_LapeRotatePoint);
addGlobalFunc('function RotatePoints(const TPA: TPointArray; A, cx, cy: Extended): TPointArray', @_LapeRotatePoints);
addGlobalFunc('function FindTPAEdges(const TPA: TPointArray): TPointArray', @_LapeFindTPAEdges);
addGlobalFunc('function ClearTPAFromTPA(const Points, PointsToRemove: TPointArray): TPointArray', @_LapeClearTPAFromTPA);
addGlobalFunc('function ReturnPointsNotInTPA(const TotalTPA: TPointArray; Box: TBox): TPointArray', @_LapeReturnPointsNotInTPA);
addGlobalFunc('function SameTPA(const aTPA, bTPA: TPointArray): Boolean', @_LapeSameTPA);
addGlobalFunc('procedure OffsetTPA(var TPA: TPointArray; Offset: TPoint)', @_LapeOffsetTPA);
addGlobalFunc('procedure OffsetATPA(var ATPA: T2DPointArray; Offset: TPoint)', @_LapeOffsetATPA);
addGlobalFunc('function PartitionTPA(const TPA: TPointArray; BoxWidth, BoxHeight: Integer): T2DPointArray', @_LapePartitionTPA);
addGlobalFunc('procedure ClearSameIntegers(var Arr: TIntegerArray)', @_LapeClearSameIntegers);
addGlobalFunc('procedure ClearSamePoints(var Arr: TPointArray)', @_LapeClearSamePoints);
addGlobalFunc('function TPAErode(const TPA: TPointArray; Amount: Integer): TPointArray', @_LapeTPAErode);
addGlobalFunc('function TPAGrow(const TPA: TPointArray; Amount: Integer): TPointArray', @_LapeTPAGrow);
addGlobalFunc('function PointsInRangeOf(const Points, Other: TPointArray; MinDist, MaxDist: Double): TPointArray; overload', @_LapePointsInRangeOf);
addGlobalFunc('function PointsInRangeOf(const Points, Other: TPointArray; MinDistX, MinDistY, MaxDistX, MaxDistY: Double): TPointArray; overload', @_LapePointsInRangeOfEx);
addGlobalFunc('function ConvexHull(const TPA: TPointArray): TPointArray', @_LapeConvexHull);

addGlobalFunc('function TPAReduceByDistance(const TPA: TPointArray; Dist: Integer): TPointArray', @_LapeTPAReduceByDistance);
addGlobalFunc('function TPAConnect(const TPA: TPointArray): TPointArray', @_LapeTPAConnect);
addGlobalFunc('function TPADensity(const TPA: TPointArray): Double;', @_LapeTPADensity);
addGlobalFunc('function TPABorder(const TPA: TPointArray): TPointArray', @_LapeTPABorder);
addGlobalFunc('function TPASkeleton(const TPA: TPointArray; FMin: Integer = 2; FMax: Integer = 6): TPointArray;', @_LapeTPASkeleton);

addGlobalFunc('function ExcludePointsDist(const TPA: TPointArray; Center: TPoint; MinDist, MaxDist: Extended): TPointArray', @_LapeExcludePointsDist);
addGlobalFunc('function ExcludePointsPolygon(const TPA: TPointArray; const Polygon: TPointArray): TPointArray', @_LapeExcludePointsPolygon);
addGlobalFunc('function ExcludePointsBox(const TPA: TPointArray; Box: TBox): TPointArray', @_LapeExcludePointsBox);

addGlobalFunc('procedure SortTPACircular(var TPA: TPointArray; Center: TPoint; StartDegrees: Integer; Clockwise: Boolean)', @_LapeSortTPACircular);

popSection();
