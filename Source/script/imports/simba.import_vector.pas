unit simba.import_vector;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportVector(Script: TSimbaScript);

implementation

uses
  lptypes, lpvartypes_record,
  simba.vector;

type
  PVector2 = ^TVector2;
  PVector3 = ^TVector3;
  PMatrix4 = ^TMatrix4;

  PVector2Array = ^TVector2Array;
  PVector3Array = ^TVector3Array;

// Vector2
procedure _LapeVector2_ToPoint(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PVector2(Params^[0])^.ToPoint();
end;

procedure _LapeVector2_ToVec3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3(Result)^ := PVector2(Params^[0])^.ToVec3(PSingle(Params^[1])^);
end;

procedure _LapeVector2_Magnitude(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PVector2(Params^[0])^.Magnitude();
end;

procedure _LapeVector2_Distance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PVector2(Params^[0])^.Distance(PVector2(Params^[1])^);
end;

procedure _LapeVector2_SqDistance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PVector2(Params^[0])^.SqDistance(PVector2(Params^[1])^);
end;

procedure _LapeVector2_Cross(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PVector2(Params^[0])^.Cross(PVector2(Params^[1])^);
end;

procedure _LapeVector2_Dot(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PVector2(Params^[0])^.Dot(PVector2(Params^[1])^);
end;

procedure _LapeVector2_Normalize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector2(Result)^ := PVector2(Params^[0])^.Normalize();
end;

procedure _LapeVector2_Rotate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector2(Result)^ := PVector2(Params^[0])^.Rotate(PSingle(Params^[1])^,PSingle(Params^[2])^, PSingle(Params^[3])^);
end;

// Vector2 Operators
procedure _LapeVector2_MUL_Vector2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector2(Result)^ := PVector2(Params^[0])^ * PVector2(Params^[1])^;
end;

procedure _LapeVector2_MUL_Single(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector2(Result)^ := PVector2(Params^[0])^ * PSingle(Params^[1])^;
end;

procedure _LapeVector2_ADD_Vector2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector2(Result)^ := PVector2(Params^[0])^ + PVector2(Params^[1])^;
end;

procedure _LapeVector2_ADD_Single(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector2(Result)^ := PVector2(Params^[0])^ + PSingle(Params^[1])^;
end;

procedure _LapeVector2_SUB_Vector2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector2(Result)^ := PVector2(Params^[0])^ - PVector2(Params^[1])^;
end;

procedure _LapeVector2_SUB_Single(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector2(Result)^ := PVector2(Params^[0])^ - PSingle(Params^[1])^;
end;

procedure _LapeSingle_SUB_Vector2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector2(Result)^ := PSingle(Params^[0])^ - PVector2(Params^[1])^;
end;

procedure _LapeVector2_DIV_Vector2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector2(Result)^ := PSingle(Params^[0])^ / PVector2(Params^[1])^;
end;

procedure _LapeVector2_DIV_Single(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector2(Result)^ := PVector2(Params^[0])^ / PSingle(Params^[1])^;
end;

procedure _LapeSingle_DIV_Vector2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector2(Result)^ := PSingle(Params^[0])^ / PVector2(Params^[1])^;
end;

// Vector3
procedure _LapeVector3_ToVec2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector2(Result)^ := PVector3(Params^[0])^.ToVec2();
end;

procedure _LapeVector3_ToPoint(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PVector3(Params^[0])^.ToPoint();
end;

procedure _LapeVector3_Magnitude(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PVector3(Params^[0])^.Magnitude();
end;

procedure _LapeVector3_Distance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PVector3(Params^[0])^.Distance(PVector3(Params^[1])^);
end;

procedure _LapeVector3_SqDistance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PVector3(Params^[0])^.SqDistance(PVector3(Params^[1])^);
end;

procedure _LapeVector3_Cross(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3(Result)^ := PVector3(Params^[0])^.Cross(PVector3(Params^[1])^);
end;

procedure _LapeVector3_Dot(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PVector3(Params^[0])^.Dot(PVector3(Params^[1])^);
end;

procedure _LapeVector3_Normalize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3(Result)^ := PVector3(Params^[0])^.Normalize();
end;

procedure _LapeVector3_Rotate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3(Result)^ := PVector3(Params^[0])^.Rotate(PSingle(Params^[1])^, PSingle(Params^[2])^, PSingle(Params^[3])^);
end;

procedure _LapeVector3_Transform(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3(Result)^ := PVector3(Params^[0])^.Transform(PMatrix4(Params^[1])^);
end;

// Vector3 operators
procedure _LapeVector3_MUL_Vector3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3(Result)^ := PVector3(Params^[0])^ * PVector3(Params^[1])^;
end;

procedure _LapeVector3_MUL_Single(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3(Result)^ := PVector3(Params^[0])^ * PSingle(Params^[1])^;
end;

procedure _LapeVector3_ADD_Vector3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3(Result)^ := PVector3(Params^[0])^ + PVector3(Params^[1])^;
end;

procedure _LapeVector3_ADD_Single(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3(Result)^ := PVector3(Params^[0])^ + PSingle(Params^[1])^;
end;

procedure _LapeVector3_SUB_Vector3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3(Result)^ := PVector3(Params^[0])^ - PVector3(Params^[1])^;
end;

procedure _LapeVector3_SUB_Single(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3(Result)^ := PVector3(Params^[0])^ - PSingle(Params^[1])^;
end;

procedure _LapeSingle_SUB_Vector3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3(Result)^ := PSingle(Params^[0])^ - PVector3(Params^[1])^;
end;

procedure _LapeVector3_DIV_Vector3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3(Result)^ := PSingle(Params^[0])^ / PVector3(Params^[1])^;
end;

procedure _LapeVector3_DIV_Single(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3(Result)^ := PVector3(Params^[0])^ / PSingle(Params^[1])^;
end;

procedure _LapeSingle_DIV_Vector3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3(Result)^ := PSingle(Params^[0])^ / PVector3(Params^[1])^;
end;

procedure _LapeVector2Array_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector2Array(Result)^ := TVector2Array.Create(PPointArray(Params^[0])^);
end;

procedure _LapeVector2Array_ToPoints(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PVector2Array(Params^[0])^.ToPoints();
end;

procedure _LapeVector2Array_Offset(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector2Array(Result)^ := PVector2Array(Params^[0])^.Offset(PVector2(Params^[1])^);
end;

procedure _LapeVector2Array_Mean(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector2(Result)^ := PVector2Array(Params^[0])^.Mean;
end;

procedure _LapeVector2Array_Rotate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector2Array(Result)^ := PVector2Array(Params^[0])^.Rotate(PSingle(Params^[1])^, PSingle(Params^[2])^, PSingle(Params^[3])^);
end;

procedure _LapeVector3Array_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3Array(Result)^ := TVector3Array.Create(PPointArray(Params^[0])^, PSingle(Params^[1])^);
end;

procedure _LapeVector3Array_ToPoints(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PVector3Array(Params^[0])^.ToPoints();
end;

procedure _LapeVector3Array_Offset(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3Array(Result)^ := PVector3Array(Params^[0])^.Offset(PVector3(Params^[1])^);
end;

procedure _LapeVector3Array_Mean(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3(Result)^ := PVector3Array(Params^[0])^.Mean;
end;

procedure _LapeVector3Array_Rotate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVector3Array(Result)^ := PVector3Array(Params^[0])^.Rotate(PSingle(Params^[1])^, PSingle(Params^[2])^, PSingle(Params^[3])^);
end;

procedure _LapeMatrix4_RotationX(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMatrix4(Result)^ := TMatrix4.RotationX(PSingle(Params^[0])^);
end;

procedure _LapeMatrix4_RotationY(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMatrix4(Result)^ := TMatrix4.RotationY(PSingle(Params^[0])^);
end;

procedure _LapeMatrix4_RotationZ(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMatrix4(Result)^ := TMatrix4.RotationZ(PSingle(Params^[0])^);
end;

procedure _LapeMatrix4_Translation(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMatrix4(Result)^ := TMatrix4.Translation(PVector3(Params^[0])^);
end;

procedure _LapeMatrix4_LookAtLH(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMatrix4(Result)^ := TMatrix4.LookAtLH(PVector3(Params^[0])^, PVector3(Params^[1])^, PVector3(Params^[2])^);
end;

procedure _LapeMatrix4_LookAtRH(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMatrix4(Result)^ := TMatrix4.LookAtRH(PVector3(Params^[0])^, PVector3(Params^[1])^, PVector3(Params^[2])^);
end;

procedure _LapeMatrix4_PerspectiveFovLH(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMatrix4(Result)^ := TMatrix4.PerspectiveFovLH(PSingle(Params^[0])^, PSingle(Params^[1])^, PSingle(Params^[2])^, PSingle(Params^[3])^);
end;

procedure _LapeMatrix4_PerspectiveFovRH(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMatrix4(Result)^ := TMatrix4.PerspectiveFovRH(PSingle(Params^[0])^, PSingle(Params^[1])^, PSingle(Params^[2])^, PSingle(Params^[3])^);
end;

procedure _LapeMatrix4_RotationQuaternion(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMatrix4(Result)^ := TMatrix4.RotationQuaternion(PSingle(Params^[0])^, PSingle(Params^[1])^, PSingle(Params^[2])^, PSingle(Params^[3])^);
end;

procedure _LapeMatrix4_RotationYawPitchRoll(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMatrix4(Result)^ := TMatrix4.RotationYawPitchRoll(PSingle(Params^[0])^, PSingle(Params^[1])^, PSingle(Params^[2])^);
end;

procedure _LapeMatrix4_MUL_Matrix4(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMatrix4(Result)^ := PMatrix4(Params^[0])^ * PMatrix4(Params^[1])^;
end;

procedure _LapeMatrix4_MUL_Single(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMatrix4(Result)^ := PMatrix4(Params^[0])^ * PSingle(Params^[1])^;
end;

procedure ImportVector(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'Vector';

    with addGlobalType('record M11, M12, M13, M14, M21, M22, M23, M24, M31, M32, M33, M34, M41, M42, M43, M44: Single; end;', 'TMatrix4') as TLapeType_Record do
      addSubDeclaration(NewGlobalVarP(@TMatrix4.IDENTITY, 'IDENTITY'));

    addGlobalType('record X,Y: Single; end;', 'TVector2');
    with addGlobalType('record X,Y,Z: Single; end;', 'TVector3') as TLapeType_Record do
    begin
      addSubDeclaration(NewGlobalVarP(@TVector3.UNIT_X, 'UNIT_X'));
      addSubDeclaration(NewGlobalVarP(@TVector3.UNIT_Y, 'UNIT_Y'));
      addSubDeclaration(NewGlobalVarP(@TVector3.UNIT_Z, 'UNIT_Z'));
    end;

    addGlobalType('array of TVector2;', 'TVector2Array');
    addGlobalType('array of TVector3;', 'TVector3Array');

    addGlobalFunc('function TVector2.ToPoint: TPoint;', @_LapeVector2_ToPoint);
    addGlobalFunc('function TVector2.ToVec3(Z: Single): TVector3;', @_LapeVector2_ToVec3);
    addGlobalFunc('function TVector2.Magnitude(): Single;', @_LapeVector2_Magnitude);
    addGlobalFunc('function TVector2.Distance(Other: TVector2): Single;', @_LapeVector2_Distance);
    addGlobalFunc('function TVector2.SqDistance(Other: TVector2): Single;', @_LapeVector2_SqDistance);
    addGlobalFunc('function TVector2.Cross(Other: TVector2): Single;', @_LapeVector2_Cross);
    addGlobalFunc('function TVector2.Dot(Other: TVector2): Single;', @_LapeVector2_Dot);
    addGlobalFunc('function TVector2.Normalize: TVector2;', @_LapeVector2_Normalize);
    addGlobalFunc('function TVector2.Rotate(Angle:Single; Mx,My:Single): TVector2;', @_LapeVector2_Rotate);

    addGlobalFunc('operator * (Left, Right: TVector2): TVector2;', @_LapeVector2_MUL_Vector2);
    addGlobalFunc('operator * (Left: TVector2; Right: Single): TVector2;', @_LapeVector2_MUL_Single);
    addGlobalFunc('operator + (Left, Right: TVector2): TVector2;', @_LapeVector2_ADD_Vector2);
    addGlobalFunc('operator + (Left: TVector2; Right: Single): TVector2;', @_LapeVector2_ADD_Single);
    addGlobalFunc('operator - (Left, Right: TVector2): TVector2;', @_LapeVector2_SUB_Vector2);
    addGlobalFunc('operator - (Left: TVector2; Right: Single): TVector2;', @_LapeVector2_SUB_Single);
    addGlobalFunc('operator - (Left: Single; Right: TVector2): TVector2;', @_LapeSingle_SUB_Vector2);
    addGlobalFunc('operator / (Left, Right: TVector2): TVector2;', @_LapeVector2_DIV_Vector2);
    addGlobalFunc('operator / (Left: TVector2; Right: Single): TVector2;', @_LapeVector2_DIV_Single);
    addGlobalFunc('operator / (Left: Single; Right: TVector2): TVector2;', @_LapeSingle_DIV_Vector2);

    addGlobalFunc('function TVector3.ToPoint: TPoint;', @_LapeVector3_ToPoint);
    addGlobalFunc('function TVector3.ToVec2: TVector2;', @_LapeVector3_ToVec2);
    addGlobalFunc('function TVector3.Magnitude(): Single;', @_LapeVector3_Magnitude);
    addGlobalFunc('function TVector3.Distance(Other: TVector3): Single;', @_LapeVector3_Distance);
    addGlobalFunc('function TVector3.SqDistance(Other: TVector3): Single;', @_LapeVector3_SqDistance);
    addGlobalFunc('function TVector3.Cross(Other: TVector3): TVector3;', @_LapeVector3_Cross);
    addGlobalFunc('function TVector3.Dot(Other: TVector3): Single;', @_LapeVector3_Dot);
    addGlobalFunc('function TVector3.Normalize: TVector3;', @_LapeVector3_Normalize);
    addGlobalFunc('function TVector3.Rotate(Angle:Single; Mx,My:Single): TVector3;', @_LapeVector3_Rotate);
    addGlobalFunc('function TVector3.Transform(Matrix: TMatrix4): TVector3;', @_LapeVector3_Transform);

    addGlobalFunc('operator * (Left, Right: TVector3): TVector3;', @_LapeVector3_MUL_Vector3);
    addGlobalFunc('operator * (Left: TVector3; Right: Single): TVector3;', @_LapeVector3_MUL_Single);
    addGlobalFunc('operator + (Left, Right: TVector3): TVector3;', @_LapeVector3_ADD_Vector3);
    addGlobalFunc('operator + (Left: TVector3; Right: Single): TVector3;', @_LapeVector3_ADD_Single);
    addGlobalFunc('operator - (Left, Right: TVector3): TVector3;', @_LapeVector3_SUB_Vector3);
    addGlobalFunc('operator - (Left: TVector3; Right: Single): TVector3;', @_LapeVector3_SUB_Single);
    addGlobalFunc('operator - (Left: Single; Right: TVector3): TVector3;', @_LapeSingle_SUB_Vector3);
    addGlobalFunc('operator / (Left, Right: TVector3): TVector3;', @_LapeVector3_DIV_Vector3);
    addGlobalFunc('operator / (Left: TVector3; Right: Single): TVector3;', @_LapeVector3_DIV_Single);
    addGlobalFunc('operator / (Left: Single; Right: TVector3): TVector3;', @_LapeSingle_DIV_Vector3);

    addGlobalFunc('function TVector2Array.Create(Points: TPointArray): TVector2Array; static;', @_LapeVector2Array_Create);
    addGlobalFunc('function TVector2Array.ToPoints: TPointArray;', @_LapeVector2Array_ToPoints);
    addGlobalFunc('function TVector2Array.Offset(Vec: TVector2): TVector2Array;', @_LapeVector2Array_Offset);
    addGlobalFunc('function TVector2Array.Mean: TVector2;', @_LapeVector2Array_Mean);
    addGlobalFunc('function TVector2Array.Rotate(Angle: Single; X,Y: Single): TVector2Array;', @_LapeVector2Array_Rotate);

    addGlobalFunc('function TVector3Array.Create(Points: TPointArray; Z: Single = 0): TVector3Array; static;', @_LapeVector3Array_Create);
    addGlobalFunc('function TVector3Array.ToPoints: TPointArray;', @_LapeVector3Array_ToPoints);
    addGlobalFunc('function TVector3Array.Offset(Vec: TVector3): TVector3Array;', @_LapeVector3Array_Offset);
    addGlobalFunc('function TVector3Array.Mean: TVector3;', @_LapeVector3Array_Mean);
    addGlobalFunc('function TVector3Array.Rotate(Angle: Single; X,Y: Single): TVector3Array;', @_LapeVector3Array_Rotate);

    addGlobalFunc('function TMatrix4.RotationX(Angle: Single): TMatrix4; static;', @_LapeMatrix4_RotationX);
    addGlobalFunc('function TMatrix4.RotationY(Angle: Single): TMatrix4; static;', @_LapeMatrix4_RotationY);
    addGlobalFunc('function TMatrix4.RotationZ(Angle: Single): TMatrix4; static;', @_LapeMatrix4_RotationZ);
    addGlobalFunc('function TMatrix4.Translation(value: TVector3): TMatrix4; static;', @_LapeMatrix4_Translation);
    addGlobalFunc('function TMatrix4.LookAtLH(eye, target, up: TVector3): TMatrix4; static;', @_LapeMatrix4_LookAtLH);
    addGlobalFunc('function TMatrix4.LookAtRH(eye, target, up: TVector3): TMatrix4; static;', @_LapeMatrix4_LookAtRH);
    addGlobalFunc('function TMatrix4.PerspectiveFovLH(fov, aspect, znear, zfar: Single): TMatrix4; static;', @_LapeMatrix4_PerspectiveFovLH);
    addGlobalFunc('function TMatrix4.PerspectiveFovRH(fov, aspect, znear, zfar: Single): TMatrix4; static;', @_LapeMatrix4_PerspectiveFovRH);
    addGlobalFunc('function TMatrix4.RotationQuaternion(x,y,z,w: Single): TMatrix4; static;', @_LapeMatrix4_RotationQuaternion);
    addGlobalFunc('function TMatrix4.RotationYawPitchRoll(yaw, pitch, roll: Single): TMatrix4; static;', @_LapeMatrix4_RotationYawPitchRoll);

    addGlobalFunc('operator * (Left, Right: TMatrix4): TMatrix4;', @_LapeMatrix4_MUL_Matrix4);
    addGlobalFunc('operator * (Left: TMatrix4; Right: Single): TMatrix4;', @_LapeMatrix4_MUL_Single);

    DumpSection := '';
  end;
end;

end.

