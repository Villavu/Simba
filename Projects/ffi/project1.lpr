program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  ffi
  { you can add units after this };

type

  { Tffi }

  Tffi = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ Tffi }

function inc_and_multiply(var i: integer) : integer;
begin
  result := i + 1;
  i := i * 2;
end;

procedure Tffi.DoRun;
var
  ErrorMsg: String;
  Cif: TFFICif;
  s: TFFIStatus;
  ret: ptruint;
  ff: integer;
  args: PPFFIType;
  values: ^Pointer;

begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  args := GetMem(sizeof(PFFIType) * 1);
//  args[0] := @ffi_type_sint32;
  args[0] := @ffi_type_pointer;


  s := ffi_prep_cif(cif, FFI_DEFAULT_ABI, 1, @ffi_type_sint32, args);
  writeln('S:' + inttostr(PtrUInt(s)));

  ff := 21;

  values := getmem(sizeof(pointer));
  values[0] := @ff;

  if s = FFI_OK then
  begin
    writeln('Woo');
    ffi_call(cif, @inc_and_multiply, @ret, @values);
    writeln('ff: ' + inttostr(ff));
    writeln('Result: ' + inttostr(ret));
  end;

  // stop program loop
  Terminate;
end;

constructor Tffi.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor Tffi.Destroy;
begin
  inherited Destroy;
end;

procedure Tffi.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: Tffi;

{$R *.res}

begin
  Application:=Tffi.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

