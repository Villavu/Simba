{$mode objfpc}{$H+}
{$macro on}
{$inline on}

{$ASMMODE INTEL}

{$OPTIMIZATION LOOPUNROLL}

{$IF FPC_VERSION >= 3}
  {$OPTIMIZATION FASTMATH}
{$ENDIF}

{$IF Defined(CPU386)}
  {$FPUTYPE SSE3}
{$ELSEIF Defined(CPUX86_64)}
  {$FPUTYPE SSE64}
{$ENDIF}

{$DEFINE callconv :=
  {$IFDEF WINDOWS}{$IFDEF CPU32}cdecl;{$ELSE}{$ENDIF}{$ENDIF}
  {$IFDEF LINUX}{$IFDEF CPU32}cdecl;{$ELSE}{$ENDIF}{$ENDIF}
}

{$modeswitch advancedrecords}
