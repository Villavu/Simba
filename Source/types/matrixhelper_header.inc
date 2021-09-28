type
  TMATRIXTYPE = array of TMATRIXARRAYTYPE;
  TMATRIXPOINTERTYPE = ^TMATRIXTYPE;
  TMATRIXHELPER = type helper for TMATRIXTYPE
    function Width: Integer;
    function Height: Integer;
    procedure SetSize(AWidth, AHeight: Integer);
    function GetValues(Indices: TPointArray): TMATRIXARRAYTYPE;
    procedure SetValues(Indices: TPointArray; Values: TMATRIXARRAYTYPE); overload;
    procedure SetValues(Indices: TPointArray; Value: TMATRIXVARTYPE); overload;
    procedure Fill(Area: TBox; Value: TMATRIXVARTYPE); overload;
    procedure Fill(Value: TMATRIXVARTYPE); overload;
    function Flatten: TMATRIXARRAYTYPE;
  end;
