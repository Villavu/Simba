addGlobalFunc('procedure _Write(S: String); override', @_LapeWrite);
addGlobalFunc('procedure _WriteLn; override', @_LapeWriteLn);

addGlobalFunc('procedure _Sort(var a: TIntegerArray); overload;', @_LapeSort_IntegerArray);
addGlobalFunc('procedure _Sort(var a: TSingleArray); overload;', @_LapeSort_SingleArray);
addGlobalFunc('procedure _Sort(var a: TDoubleArray); overload;', @_LapeSort_DoubleArray);
addGlobalFunc('procedure _Sort(var a: TExtendedArray); overload;', @_LapeSort_ExtendedArray);

addGlobalFunc('function _Unique(const a: TPointArray): TPointArray; overload', @_LapeUnique_PointArray);
addGlobalFunc('function _Unique(const a: TIntegerArray): TIntegerArray; overload', @_LapeUnique_IntegerArray);
addGlobalFunc('function _Unique(const a: TSingleArray): TSingleArray; overload', @_LapeUnique_SingleArray);
addGlobalFunc('function _Unique(const a: TDoubleArray): TDoubleArray; overload', @_LapeUnique_DoubleArray);
addGlobalFunc('function _Unique(const a: TExtendedArray): TExtendedArray; overload', @_LapeUnique_ExtendedArray);
addGlobalFunc('function _Unique(const a: TStringArray): TStringArray; overload', @_LapeUnique_StringArray);

addGlobalFunc('function _IndicesOf(const Item: String; const a: TStringArray): TIntegerArray; overload', @_LapeIndicesOf_StringArray);
addGlobalFunc('function _IndicesOf(const Item: TPoint; const a: TPointArray): TIntegerArray; overload', @_LapeIndicesOf_PointArray);
addGlobalFunc('function _IndicesOf(const Item: Integer; const a: TIntegerArray): TIntegerArray; overload', @_LapeIndicesOf_IntegerArray);
addGlobalFunc('function _IndicesOf(const Item: Single; const a: TSingleArray): TIntegerArray; overload', @_LapeIndicesOf_SingleArray);
addGlobalFunc('function _IndicesOf(const Item: Double; const a: TDoubleArray): TIntegerArray; overload', @_LapeIndicesOf_DoubleArray);
addGlobalFunc('function _IndicesOf(const Item: Extended; const a: TExtendedArray): TIntegerArray; overload', @_LapeIndicesOf_ExtendedArray);

addGlobalFunc('function _IndexOf(const Item: String; const a: TStringArray): Integer; overload', @_LapeIndexOf_StringArray);
addGlobalFunc('function _IndexOf(const Item: TPoint; const a: TPointArray): Integer; overload', @_LapeIndexOf_PointArray);
addGlobalFunc('function _IndexOf(const Item: Integer; const a: TIntegerArray): Integer; overload', @_LapeIndexOf_IntegerArray);
addGlobalFunc('function _IndexOf(const Item: Single; const a: TSingleArray): Integer; overload', @_LapeIndexOf_SingleArray);
addGlobalFunc('function _IndexOf(const Item: Double; const a: TDoubleArray): Integer; overload', @_LapeIndexOf_DoubleArray);
addGlobalFunc('function _IndexOf(const Item: Extended; const a: TExtendedArray): Integer; overload', @_LapeIndexOf_ExtendedArray);

addGlobalFunc('function _ArraySum(const a: TPointArray): TPoint; overload', @_LapeArraySum_PointArray);
addGlobalFunc('function _ArraySum(const a: TIntegerArray): Int64; overload', @_LapeArraySum_IntegerArray);
addGlobalFunc('function _ArraySum(const a: TSingleArray): Extended; overload', @_LapeArraySum_SingleArray);
addGlobalFunc('function _ArraySum(const a: TDoubleArray): Extended; overload', @_LapeArraySum_DoubleArray);
addGlobalFunc('function _ArraySum(const a: TExtendedArray): Extended; overload', @_LapeArraySum_ExtendedArray);

addGlobalFunc('function _ArrayMin(const a: TIntegerArray): Int64; overload', @_LapeArrayMin_IntegerArray);
addGlobalFunc('function _ArrayMin(const a: TSingleArray): Single; overload', @_LapeArrayMin_SingleArray);
addGlobalFunc('function _ArrayMin(const a: TDoubleArray): Double; overload', @_LapeArrayMin_DoubleArray);
addGlobalFunc('function _ArrayMin(const a: TExtendedArray): Extended; overload', @_LapeArrayMin_ExtendedArray);

addGlobalFunc('function _ArrayMax(const a: TIntegerArray): Int64; overload', @_LapeArrayMax_IntegerArray);
addGlobalFunc('function _ArrayMax(const a: TSingleArray): Single; overload', @_LapeArrayMax_SingleArray);
addGlobalFunc('function _ArrayMax(const a: TDoubleArray): Double; overload', @_LapeArrayMax_DoubleArray);
addGlobalFunc('function _ArrayMax(const a: TExtendedArray): Extended; overload', @_LapeArrayMax_ExtendedArray);
