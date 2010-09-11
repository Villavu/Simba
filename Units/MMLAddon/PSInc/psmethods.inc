

function ps_Format(const fmt : string;const args : array of const) : string; extdecl;
begin;
  Result := Format(fmt,Args);
end;

function MakeString(data : TPSVariantIFC) : string;
begin;
  if data.Dta = nil then
    result := 'Nil'
  else
  if data.aType.basetype in [btString,btChar] then
    result := PSGetAnsiString(Data.Dta,data.aType)
  else if data.aType.ExportName = 'BOOLEAN' then
    result := BoolToStr(PSGetInt(Data.Dta,data.aType) <> 0,true)
  else if data.aType.BaseType in [btExtended,btSingle,btDouble] then
    result := FloatToStr(PSGetReal(data.Dta,data.aType))
  else
    result := PSVariantToString(data,'');
end;

function writeln_(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  Result:=true;
  psWriteln(makeString(NewTPSVariantIFC(Stack[Stack.Count-1],false)));
end;

function swap_(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  Param1,Param2: TPSVariantIFC;
  tempCopy : pointer;
begin
  Result:=true;
  Param1 := NewTPSVariantIFC(Stack[Stack.count-1],true);
  Param2 := NewTPSVariantIFC(Stack[Stack.count-2],true);
  if Param1.aType.BaseType <> Param2.aType.BaseType then
    exit(false)
  else
  begin
    Param1.aType.CalcSize;
    param2.aType.CalcSize;
    if Param1.aType.RealSize <> Param2.aType.RealSize then
      exit(false);
    GetMem(tempcopy,Param1.aType.RealSize);
    Move(Param1.Dta^,tempCopy^,param1.atype.realsize);
    Move(Param2.Dta^,Param1.Dta^,param1.atype.realsize);
    Move(tempCopy^,Param2.Dta^,param1.atype.realsize);
    Freemem(tempcopy);
  end;
end;

function ToStr_(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  result := true;
  Stack.SetAnsiString(-1, MakeString(NewTPSVariantIFC(Stack[Stack.Count-2],false)));
end;
