unit mtcpuinfo;
interface

//returns number of logical cores: a computer with two hyperthreaded cores will report 4
function GetSystemThreadCount: Integer;
 
implementation
 
{$IF defined(windows)}
uses windows;
{$endif}

{$IF defined(darwin)}
uses ctypes, sysctl;
{$endif} 
 
{$IFDEF Linux}
uses ctypes;
 
const _SC_NPROCESSORS_ONLN = 83;
function sysconf(i: cint): clong; cdecl; external name 'sysconf';
{$ENDIF}
 
 
function GetSystemThreadCount: Integer;
// returns a good default for the number of threads on this system
{$IF defined(windows)}
//returns total number of processors available to system including logical hyperthreaded processors
var
  i: Integer;
  ProcessAffinityMask, SystemAffinityMask: DWORD_PTR;
  Mask: DWORD;
  SystemInfo: SYSTEM_INFO;
begin
  if GetProcessAffinityMask(GetCurrentProcess, ProcessAffinityMask, SystemAffinityMask)
  then begin
    Result := 0;
    for i := 0 to 31 do begin
      Mask := DWord(1) shl i;
      if (ProcessAffinityMask and Mask)<>0 then
        inc(Result);
    end;
  end else begin
    //can't get the affinity mask so we just report the total number of processors
    GetSystemInfo(SystemInfo);
    Result := SystemInfo.dwNumberOfProcessors;
  end;
end;
{$ELSEIF defined(UNTESTEDsolaris)}
  begin
    t = sysconf(_SC_NPROCESSORS_ONLN);
  end;
{$ELSEIF defined(freebsd) or defined(darwin)}
var
  mib: array[0..1] of cint;
  len: cint;
  t: cint;
begin
  mib[0] := CTL_HW;
  mib[1] := HW_NCPU;
  len := SizeOf(t);
  fpsysctl(pchar(@mib), 2, @t, @len, Nil, 0);
  Result := t;
end;
{$ELSEIF defined(linux)}
  begin
    Result := sysconf(_SC_NPROCESSORS_ONLN);
  end;
 
{$ELSE}
  begin
    Result := 1;
  end;
{$ENDIF}

end.
