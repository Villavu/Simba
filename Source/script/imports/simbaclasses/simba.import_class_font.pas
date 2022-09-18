unit simba.import_class_font;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.fontloader, simba.ocrutil;

procedure _LapeMFont_Name_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PMFont(Params^[0])^.Name;
end;

procedure _LapeMFont_Name_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFont(Params^[0])^.Name := PString(Params^[1])^;
end;

procedure _LapeMFont_Data_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POcrData(Result)^ := PMFont(Params^[0])^.Data;
end;

procedure _LapeMFont_Data_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFont(Params^[0])^.Data := POcrData(Params^[1])^;
end;

procedure _LapeMFont_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFont(Params^[0])^ := TMFont.Create();
end;

procedure _LapeMFont_Copy(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFont(Result)^ := PMFont(Params^[0])^.Copy();
end;

procedure _LapeMFont_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFont(Params^[0])^.Free();
end;

procedure ImportFont(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TMFont');
    addGlobalType('record xoff, yoff, width, height, index: Int32; inited: boolean; end', 'TOCRGlyphMetric');
    addGlobalType('record ascii: array[0..255] of TOCRGlyphMetric; pos: T2DIntegerArray; pos_adj: array of Double; neg: T2DIntegerArray; neg_adj: array of Double; map: array of char; width, height, max_width, max_height, inputs, outputs: integer; end', 'TOCRData');
    addGlobalType('array of TOCRData', 'TOCRDataArray');
    addClassVar('TMFont', 'Name', 'String', @_LapeMFont_Name_Read, @_LapeMFont_Name_Write);
    addClassVar('TMFont', 'Data', 'TOcrData', @_LapeMFont_Data_Read, @_LapeMFont_Data_Write);
    addGlobalFunc('procedure TMFont.Init', @_LapeMFont_Init);
    addGlobalFunc('function TMFont.Copy: TMFont', @_LapeMFont_Copy);
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportFont);

end.

