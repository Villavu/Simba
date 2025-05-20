unit simba.script_importutil;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.target, simba.image;

// Note: Lape objects internally are just dynarray of byte

// These all require the object declared as such
// ```
//   object
//     Instance: Pointer;
//     DontManage: Boolean;
//   end;
// ```

type
  TLapeObject = array of Byte;
  PLapeObject = ^TLapeObject;

operator := (Target: TSimbaTarget): TLapeObject;
operator := (Image: TSimbaImage): TLapeObject;

function IsManaging(Obj: PLapeObject): Boolean;
procedure SetManaging(Obj: PLapeObject; Value: Boolean);

implementation

type
  PSimbaTarget = ^TSimbaTarget;
  PSimbaImage = ^TSimbaImage;

operator := (Target: TSimbaTarget): TLapeObject;
begin
  SetLength(Result, SizeOf(Pointer) + SizeOf(Boolean));
  PSimbaTarget(@Result[0])^ := Target;
end;

operator := (Image: TSimbaImage): TLapeObject;
begin
  SetLength(Result, SizeOf(Pointer) + SizeOf(Boolean));
  PSimbaImage(@Result[0])^ := Image;
end;

function IsManaging(Obj: PLapeObject): Boolean;
begin
  Result := not PBoolean(@Obj^[SizeOf(Pointer)])^;
end;

procedure SetManaging(Obj: PLapeObject; Value: Boolean);
begin
  PBoolean(@Obj^[SizeOf(Pointer)])^ := not Value;
end;

end.

