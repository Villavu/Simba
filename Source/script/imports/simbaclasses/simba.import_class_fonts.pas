unit simba.import_class_fonts;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, graphics, lptypes,
  simba.script_compiler, simba.fontloader, simba.ocrutil;

type
  PObject = ^TObject;

procedure _LapeMFonts_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFonts(Params^[0])^ := TMFonts.Create(PObject(Params^[1])^);
end;

procedure _LapeMFonts_GetFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POcrData(Result)^ := PMFonts(Params^[0])^.GetFontData(PString(Params^[1])^);
end;

procedure _LapeMFonts_FreeFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFonts(Params^[0])^.FreeFont(PString(Params^[1])^);
end;

procedure _LapeMFonts_LoadFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMFonts(Params^[0])^.LoadFont(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeMFonts_LoadSystemFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMFonts(Params^[0])^.LoadSystemFont(TFont(Params^[1]^), PString(Params^[2])^);
end;

procedure _LapeMFonts_Copy(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFonts(Result)^ := PMFonts(Params^[0])^.Copy(PObject(Params^[1])^);
end;

procedure _LapeMFonts_Count(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PMFonts(Params^[0])^.Count();
end;

procedure _LapeMFonts_Path_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PMFonts(Params^[0])^.Path;
end;

procedure _LapeMFonts_Path_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFonts(Params^[0])^.Path := PString(Params^[1])^;
end;

procedure _LapeMFonts_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFonts(Params^[0])^.Free();
end;

procedure ImportFonts(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Classes');

    addClass('TMFonts');
    addGlobalFunc('procedure TMFonts.Init(Owner : TObject)', @_LapeMFonts_Init);
    addGlobalFunc('function TMFonts.GetFont(const Name: String): TOcrData;', @_LapeMFonts_GetFont);
    addGlobalFunc('function TMFonts.FreeFont(const Name: String): Boolean;', @_LapeMFonts_FreeFont);
    addGlobalFunc('function TMFonts.LoadFont(const Name: String; Shadow: Boolean): boolean;', @_LapeMFonts_LoadFont);
    addGlobalFunc('function TMFonts.LoadSystemFont(const SysFont : TFont; const FontName : string): boolean;', @_LapeMFonts_LoadSystemFont);
    addGlobalFunc('function TMFonts.Copy(Owner : TObject): TMFonts;', @_LapeMFonts_Copy);
    addGlobalFunc('function TMFonts.Count: integer;', @_LapeMFonts_Count);
    addClassVar('TMFonts', 'Path', 'string', @_LapeMFonts_Path_Read, @_LapeMFonts_Path_Write);
    //addGlobalFunc('procedure TMFonts.Free;', @_LapeMFonts_Free);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportFonts);

end.

