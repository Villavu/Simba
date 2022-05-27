unit simba.import_events;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lpeval,
  simba.script_compiler;

procedure ImportEvents(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Events');

    addDelayedCode([
      LapeDelayedFlags,
      'var',
      '  _TerminateEvents, _UserTerminateEvents, _PauseEvents, _ResumeEvents: array of record',
      '    Proc: procedure;',
      '    Method: procedure of object;',
      '  end;',
      '',
      'procedure _CallOnTerminate;',
      'var I: Integer;',
      'begin',
      '  for I := 0 to High(_TerminateEvents) do',
      '  begin',
      '    if Assigned(_TerminateEvents[I].Proc) then _TerminateEvents[I].Proc();',
      '    if Assigned(_TerminateEvents[I].Method) then _TerminateEvents[I].Method();',
      '  end;',
      'end;',
      '',
      'procedure _CallOnUserTerminate;',
      'var I: Integer;',
      'begin',
      '  for I := 0 to High(_UserTerminateEvents) do',
      '  begin',
      '    if Assigned(_UserTerminateEvents[I].Proc) then _UserTerminateEvents[I].Proc();',
      '    if Assigned(_UserTerminateEvents[I].Method) then _UserTerminateEvents[I].Method();',
      '  end;',
      'end;',
      '',
      'procedure _CallOnPause;',
      'var I: Integer;',
      'begin',
      '  for I := 0 to High(_PauseEvents) do',
      '  begin',
      '    if Assigned(_PauseEvents[I].Proc) then _PauseEvents[I].Proc();',
      '    if Assigned(_PauseEvents[I].Method) then _PauseEvents[I].Method();',
      '  end;',
      'end;',
      '',
      'procedure _CallOnResume;',
      'var I: Integer;',
      'begin',
      '  for I := 0 to High(_ResumeEvents) do',
      '  begin',
      '    if Assigned(_ResumeEvents[I].Proc) then _ResumeEvents[I].Proc();',
      '    if Assigned(_ResumeEvents[I].Method) then _ResumeEvents[I].Method();',
      '  end;',
      'end;'
    ], '!Events');

    addDelayedCode([
      LapeDelayedFlags,
      'procedure AddOnTerminate(Proc: procedure); overload;',
      'begin',
      '  _TerminateEvents += [Proc];',
      'end;',
      '',
      'procedure AddOnTerminate(Proc: procedure of object); overload;',
      'begin',
      '  _TerminateEvents += [,Proc];',
      'end;',
      '',
      'procedure AddOnUserTerminate(Proc: procedure); overload;',
      'begin',
      '  _UserTerminateEvents += [Proc];',
      'end;',
      '',
      'procedure AddOnUserTerminate(Proc: procedure of object); overload;',
      'begin',
      '  _UserTerminateEvents += [,Proc];',
      'end;',
      '',
      'procedure AddOnPause(Proc: procedure); overload;',
      'begin',
      '  _PauseEvents += [Proc];',
      'end;',
      '',
      'procedure AddOnPause(Proc: procedure of object); overload;',
      'begin',
      '  _PauseEvents += [,Proc];',
      'end;',
      '',
      'procedure AddOnResume(Proc: procedure); overload;',
      'begin',
      '  _ResumeEvents += [Proc];',
      'end;',
      '',
      'procedure AddOnResume(Proc: procedure of object); overload;',
      'begin',
      '  _ResumeEvents += [,Proc];',
      'end;'
    ]);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportEvents);

end.

