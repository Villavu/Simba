program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  ffi, ctypes
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

procedure testcall;
var
  Cif: TFFICif;
  s: TFFIStatus;
  ret: ptruint;
  ff: integer;
  args: PPFFIType;
  values: ^Pointer;
begin
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
end;

procedure puts(var cif: TFFICif; var ret: cuint; var args: TPointerArray; userdata: Pointer) cdecl;
var
  s: PChar;
begin
  writeln('Userdata:', QWord(userdata));
  s := PChar(args[0]);
  writeln(s);
  ret := strlen(s);
end;

procedure testclosure;
var
  cif: TFFICif;
  s: TFFIStatus;
  args: PPFFIType;
  closure: PFFIClosure;
  bound_puts: function (s: pchar): PtrUInt;
  rc: integer;
begin
  closure := ffi_closure_alloc(sizeof(TFFIClosure), @bound_puts);

  if assigned(closure) then
  begin
    args := GetMem(sizeof(PFFIType) * 1);
    args[0] := @ffi_type_pointer;
    s := ffi_prep_cif(cif, FFI_DEFAULT_ABI, 1, @ffi_type_uint32, args);
    if s = FFI_OK then
    begin
      s := ffi_prep_closure_loc(closure, &cif, @puts, Pointer(42), bound_puts);
      if s = FFI_OK then
      begin
        rc := bound_puts('Hello World!');
        writeln('Return value:', rc);
      end else writeln('ffi_prep_closure_loc failed', s);
    end else writeln('ffi_prep_cif failed', s);
  end else writeln('Closure not assigned');

  ffi_closure_free(closure);
end;

procedure Tffi.DoRun;
var
  ErrorMsg: String;

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

  //testcall;
  testclosure;

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

