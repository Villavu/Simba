{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Utils for lape objects which we use for management of imported classes.
}
unit simba.script_objectutil;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  lpcompiler, lptypes,
  simba.target,
  simba.target_asyncmovemouse,
  simba.image,
  simba.httpclient,
  simba.json,
  simba.resource;

{
 Lape objects internally are just dynarray of byte.
 We declare these objects as such:
   object
     Instance: Pointer;
     DontManage: Boolean;
   end;
 As the instance pointer is the first field we can just double pointer to access
}

const
  LapeObjectSize = SizeOf(Pointer) + SizeOf(Boolean);
  LapeObjectDontManageOffset = SizeOf(Pointer);

type
  TLapeObject = array of Byte;
  PLapeObject = ^TLapeObject;

  PLapeObjectHTTPClient = ^PSimbaHTTPClient;
  PLapeObjectImage = ^PSimbaImage;
  PLapeObjectTarget = ^PSimbaTarget;
  PLapeObjectJSON = ^PSimbaJSONItem;
  PLapeObjectResourceWriter = ^PSimbaResourceWriter;
  PLapeObjectResourceReader = ^PSimbaResourceReader;
  PLapeObjectASyncMouse = ^PASyncMouse;

function IsLapeObjectManage(const Obj: PLapeObject): Boolean;
procedure SetLapeObjectManage(const Obj: PLapeObject; const Value: Boolean);

procedure LapeObjectImport(const Compiler: TLapeCompiler; const Name: lpString);
procedure LapeObjectDestroy(const Obj: PLapeObject);
function LapeObjectAlloc(Instance: Pointer; Manage: Boolean = True): TLapeObject;

implementation

function IsLapeObjectManage(const Obj: PLapeObject): Boolean;
begin
  Result := not PBoolean(@Obj^[LapeObjectDontManageOffset])^;
end;

procedure SetLapeObjectManage(const Obj: PLapeObject; const Value: Boolean);
begin
  PBoolean(@Obj^[LapeObjectDontManageOffset])^ := not Value;
end;

procedure LapeObjectImport(const Compiler: TLapeCompiler; const Name: lpString);
begin
  Compiler.addGlobalType('object {%CODETOOLS OFF} Instance: Pointer; DontManage: Boolean; {%CODETOOLS ON} end;', Name);
end;

// just cast to TObject and call destructor
procedure LapeObjectDestroy(const Obj: PLapeObject);
type
  PObject = ^TObject;
  PPObject = ^PObject;
begin
  if IsLapeObjectManage(Obj) and Assigned(PPObject(Obj)^^) then
    FreeAndNil(PPObject(Obj)^^);
end;

// Should never really need to use this
function LapeObjectAlloc(Instance: Pointer; Manage: Boolean): TLapeObject;
begin
  SetLength(Result, LapeObjectSize);

  PPointer(@Result[0])^ := Instance;
  if not Manage then
    PBoolean(@Result[LapeObjectDontManageOffset])^ := True;
end;

end.

