type
  Integer = Integer;
  Byte = Byte;
  String = String;
  Char = Char;

type
  TPoint = record X, Y: Integer; end;
  TPointArray = array of TPoint;
  T2DPointArray = array of TPointArray;

  TBox = record X1, Y1, X2, Y2: Integer; end;

  TColorBGRA = record B,G,R,A: Byte; end;
  PColorBGRA = ^TColorBGRA;

function TBox.Center: TPoint;
begin
end;

function TPoint.Rand(Amount: Integer): TPoint;
begin
end;
{
function GetBox: TBox;
begin
end;

function GetColor: PColorBGRA;
begin
end;

function GetAnonRec: record box: TBox; point: TPoint; end;
begin
end;

function GetAnonRecArray: array of record box: TBox; point: TPoint; end;
begin
end;

function GetTPA: TPointArray;
begin
end;

function GetATPA: T2DPointArray;
begin
end;

var
  AnonRecord: record
    NestedAnonRecord: record
      NestedField1, NestedField2: Integer;
    end;
    Field1: Integer;
  end;

var
  Colors: array of PColorBGRA;

var
  Str: String;
  Strings: array of array of String;

type
  TFunc = function(a,b,c: Integer): Boolean;
  TFuncNative = native(TFunc);

var
  func: TFuncNative;

type
  TLazObject = type Pointer;
  TLazForm = type TLazObject;

procedure TLazObject.A;
begin
end;

procedure TLazForm.B;
begin
end;

var form: TLazForm;
}
