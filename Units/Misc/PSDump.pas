{
@abstract(Component wrapper for IFPS3 compiler and executer)
A component wrapper for IFPS3, including debugging support.

}

{$IFDEF FPC}
  {$H+}
{$ENDIF}

unit PSDump;

interface

uses
  {$IFNDEF LINUX} Windows, {$ENDIF} SysUtils, Classes, uPSRuntime, uPSUtils, uPSComponent,
  uPSCompiler, uPSC_dll, typInfo;

type
  TPSOnCompCleanup = function (Sender: TObject; aComp: TPSPascalCompiler): Boolean of object;
  TPSOnException = procedure (Sender: TPSExec; ExError: TPSError; const ExParam: tbtstring; ExObject: TObject; ProcNo, Position: Cardinal) of object;

  TPSScriptExtension = class(TPSScriptDebugger)
  private
    FOnBeforeCleanUp: TPSOnCompCleanup;
    FNeedCompiling: Boolean;
    FOnScriptChange: TNotifyEvent;
    FOnException: TPSOnException;
    fItems: TStrings;
    
    procedure GetCodeProps;
  protected
    function DoBeforeCleanup(Sender: TObject; aComp: TPSPascalCompiler):Boolean;
    procedure DoScriptChange(sender:TObject);
  public
    constructor Create(AOwner: TComponent); override;
    function Compile: Boolean; override;
    function Execute: Boolean; override;
    procedure GetValueDefs(aItems: TStrings);
    procedure CompileIfNeeded;

    property NeedCompiling : Boolean read FNeedCompiling;
  published
    property OnBeforeCleanUp: TPSOnCompCleanup read FOnBeforeCleanUp write FOnBeforeCleanUp;
    Property OnScriptChange: TNotifyEvent read FOnScriptChange write fOnScriptChange;
    property OnException: TPSOnException read FOnException write FOnException;
  end;


implementation

function BeforeCleanup(Sender: TPSPascalCompiler): Boolean;
begin
  Result := TPSScriptExtension(Sender.ID).DoBeforeCleanUp(TObject(Sender.ID),Sender);
end;

procedure CEException(Sender: TPSExec; ExError: TIFError; const ExParam: tbtstring; ExObject: TObject; ProcNo, Position: Cardinal);
begin
  if (TPSScriptExtension(Sender.ID).FOnException <> nil) then
    TPSScriptExtension(Sender.ID).FOnException(Sender, ExError, ExParam, ExObject, ProcNo, Position);
end;

{ TPSScriptExtension }

function TPSScriptExtension.Compile: Boolean;
begin
  Result := inherited Compile;
  FNeedCompiling := not result;
end;

constructor TPSScriptExtension.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Comp.OnBeforeCleanup := {$IFDEF FPC}@{$ENDIF}BeforeCleanup;
  Exec.OnException     := {$IFDEF FPC}@{$ENDIF}CEException;

  TStringList(script).OnChange := {$IFDEF FPC}@{$ENDIF}DoScriptChange;
  FNeedCompiling := True;
end;

type
  _TMyPascalCompiler = class(TPSPascalCompiler);
procedure TPSScriptExtension.GetCodeProps;

  function TypeToString(t: TPSType; Definition: Boolean = False): string; forward;

  function FunctionType(f: TObject): string;
  const
    FuncStr: array[Boolean] of string = ('procedure', 'function');
  begin
    if (f is TPSDelphiClassItemConstructor) then
      Result := 'constructor'
    else if (f is TPSDelphiClassItemMethod) then
      Result := FuncStr[TPSDelphiClassItemMethod(f).Decl.Result <> nil]
    else if (f is TPSProceduralType) then
      Result := FuncStr[TPSProceduralType(f).ProcDef.Result <> nil]
    else if (f is TPSRegProc) then
      Result := FuncStr[TPSRegProc(f).Decl.Result <> nil]
    else if (f is TPSInternalProcedure) then
      Result := FuncStr[TPSInternalProcedure(f).Decl.Result <> nil]
    else
      Result := '';
  end;

  function ParamsToString(d: TPSParametersDecl; CheckResult: Boolean = True): string;
  const
    ParamStr: array [pmIn..pmInOut] of tbtstring = ('','out ','var ');
  var
    i: Integer;
  begin
    Result := '';
    for i := 0 to d.ParamCount - 1 do
    begin
      if (Result <> '') then
        Result := Result + '; ';
      Result := Result + ParamStr[d.Params[i].Mode] + d.Params[i].OrgName;
      if (d.Params[i].aType <> nil) then
        Result := Result +': '+ TypeToString(d.Params[i].aType);
    end;

    if (Result <> '') then
      Result := '('+Result+')';
    if CheckResult and (d.Result <> nil) then
      Result := Result + ': ' + TypeToString(d.Result);
  end;

  function ClassItemsToString(c: TPSCompileTimeClass; DoneList: TStrings = nil): string;
  var
    ci: TPSDelphiClassItem;
    i, ii: Integer;
    s: string;
    Def: PtrUInt;
    ListFree: Boolean;
  begin
    Result := '';
    if (c = nil) then
      Exit;

    if (DoneList = nil) then
    begin
      DoneList := TStringList.Create;
      TStringList(DoneList).Sorted := True;
      ListFree := True;
    end
    else
      ListFree := False;

    if (not c.Property_Find('', Def)) then
      Def := 0;

    for i := 0 to c.Count - 1 do
    begin
      ci := c.Items[i];
      If (ci = nil) or (DoneList.IndexOf(ci.OrgName) > -1) then
        Continue;
      DoneList.Add(ci.OrgName);

      if (ci is TPSDelphiClassItemConstructor) or (ci is TPSDelphiClassItemMethod) then
        Result := Result + FunctionType(ci) + ' ' + ci.OrgName + ParamsToString(ci.Decl, not (ci is TPSDelphiClassItemConstructor)) + '; '
      else if (ci is TPSDelphiClassItemProperty) then
      begin
        s := '';
        for ii := 0 to ci.Decl.ParamCount - 1 do
        begin
          if (s <> '') then
            s := s + ', ';
          s := s + 'Index';
          if (ii > 0) then
            s := s + IntToStr(ii + 1);
          if (ii = ci.Decl.ParamCount - 1) then
            s := s + ': ' + TypeToString(ci.Decl.Params[ii].aType);
        end; 
        if (s <> '') then
          s := '['+s+']';
        Result := Result + 'property ' + ci.OrgName + s;
        if (ci.Decl.Result <> nil) then
          Result := Result + ': '+ TypeToString(ci.Decl.Result);
        if (PtrUInt(ci) = Def) then
          Result := Result + '; default';
        Result := Result + '; ';
      end;
    end;

    Result := Result + ClassItemsToString(c.ClassInheritsFrom, DoneList);
    if ListFree then
      DoneList.Free;
  end;

  function TypeToString(t: TPSType; Definition: Boolean = False): string;
  var
    r: PIFPSRecordFieldTypeDef;
    i: Integer;
  begin
    if (not Definition) and (t.OriginalName <> '') and (t.OriginalName[1] <> '!') then
      Result := t.OriginalName
    else if (t is TPSRecordType) then
    begin
      Result := 'record ';
      for i := 0 to (t as TPSRecordType).RecValCount - 1 do
      begin
        r := (t as TPSRecordType).RecVal(i);
        Result := Result + r.FieldOrgName + ': ' + TypeToString(r.aType) + '; ';
      end;
      Result := Result + 'end';
    end
    else if (t is TPSClassType) then
      Result := 'class ' + ClassItemsToString((t as TPSClassType).Cl) + 'end'
    else if (t is TPSProceduralType) then
      Result := FunctionType(t) + ParamsToString((t as TPSProceduralType).ProcDef)
    else if (t is TPSArrayType) then
      Result := 'array of ' + TypeToString((t as TPSArrayType).ArrayTypeNo)
    else if (t is TPSStaticArrayType) then
      with (t as TPSStaticArrayType) do
        Result := 'array[' + IntToStr(StartOffset) + '..' + IntToStr(StartOffset + Length) + ' of ' + TypeToString(ArrayTypeNo)
    else if (t is TPSSetType) then
      Result := 'set of ' + TypeToString((t as TPSSetType).SetType)
    else if (t is TPSTypeLink) then
      Result := TypeToString((t as TPSTypeLink).LinkTypeNo)
    else if (t is TPSEnumType) then
    begin
      Result := '';
      for i := 0 to t.Attributes.Count - 1 do
      begin
        if (Result <> '') then
          Result := Result + ', ';
        Result := Result + TPSConstant(t.Attributes[i].AType).OrgName;
      end;

      if (Result <> '') then
        Result := '('+Result+')'
      else
        Result := 'enum';
    end
    else
      Result := t.OriginalName;
  end;

  function IfRVariantToString(v: TIfRVariant): string;
  begin
    case v.FType.BaseType of
      btU8: Result := IntToStr(v.tu8);         
      btS8: Result := IntToStr(v.ts8);
      btU16: Result := IntToStr(v.tu16);         
      btS16: Result := IntToStr(v.ts16);        
      btU32: Result := IntToStr(v.tu32);            
      btS32: Result := IntToStr(v.ts32);             
      btSingle: Result := FloatToStr(v.tsingle);          
      btDouble: Result := FloatToStr(v.tdouble);         
      btExtended: Result := FloatToStr(v.textended);        
      btString: Result := MakeString(tbtString(v.tstring));
    {$IFNDEF PS_NOINT64}
      btS64: Result := IntToStr(v.ts64);            
    {$ENDIF}
      btChar: Result := '#' + IntToStr(Ord(v.tchar));            
    {$IFNDEF PS_NOWIDESTRING}
      btWideString: Result := tbtwidestring(v.twidestring);     
      btWideChar: Result := '#' + IntToStr(Ord(v.twidechar));      
    {$ENDIF}          
      btCurrency: Result := FloatToStr(v.tcurrency);             
      btUnicodeString: Result := tbtunicodestring(v.tunistring);
      else
        Result := v.FType.OriginalName;
    end;
  end;

var
  i : Integer;
begin
  if (fItems = nil) then
    Exit;
    
  fItems.BeginUpdate;
  try
    fItems.Clear;

    with _TMyPascalCompiler(Comp) do
    begin
      for i := 0 to FConstants.Count - 1 do
        with TPSConstant(FConstants[i]) do
          if (Value^.FType is TPSEnumType) then
            Value^.FType.Attributes.Add(TPSAttributeType(FConstants[i]))
          else
            fItems.Add('const ' + OrgName + ' = ' + IfRVariantToString(Value^) + ';');

      for i := 0 to FTypes.Count - 1 do
        with TPSType(FTypes[i]) do
          if (OriginalName <> '') and (OriginalName[1] <> '!') then
            fItems.Add('type ' + OriginalName + ' = ' + TypeToString(TPSType(FTypes[i]), True) + ';');

      for i := 0 to FVars.Count - 1 do
        with TPSVar(FVars[i]) do
          fItems.Add('var ' + OrgName + ': ' + TypeToString(TPSType(FVars[i]), True) + ';');

      for i := 0 to FRegProcs.Count - 1 do
        with TPSRegProc(FRegProcs[i]) do
          fItems.Add(FunctionType(TObject(FRegProcs[i])) + ' ' + OrgName + ParamsToString(Decl) + '; forward;');

      for i := 0 to FProcs.Count - 1 do
        if (TPSProcedure(FProcs[i]) is TPSInternalProcedure) then        
          with TPSInternalProcedure(FProcs[i]) do
            if (Name <> '') and (Name[1] <> '!') then  
              fItems.Add(FunctionType(TObject(FProcs[i])) + ' ' + OriginalName + ParamsToString(Decl) + '; forward;');
    end;
  finally
    fItems.EndUpdate;
  end;
end;

procedure TPSScriptExtension.GetValueDefs(aItems: TStrings);
begin
  fItems := aItems;
  try
    Compile;
  finally
    fItems := nil;
  end;
end;

function TPSScriptExtension.DoBeforeCleanup(Sender: TObject; aComp: TPSPascalCompiler): Boolean;
begin
  Result := True;
  if (fItems <> nil) then
    GetCodeProps;
  if (FOnBeforeCleanUp <> nil) then
    Result := FOnBeforeCleanUp(Sender, aComp);
end;

function TPSScriptExtension.Execute: Boolean;
begin
  CompileIfNeeded;
  Result := inherited Execute;
end;

procedure TPSScriptExtension.DoScriptChange(sender: TObject);
begin
  FNeedCompiling := True;
  if (FOnScriptChange <> nil) then
    FOnScriptChange(sender);
end;

procedure TPSScriptExtension.CompileIfNeeded;
begin
  if FNeedCompiling then
    Compile;
end;

end.
