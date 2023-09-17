{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
{$mode ObjFPC}{$H+}

{$IFOPT D+}
  {$DEFINE SIMBA_HAS_DEBUGINFO}
{$ELSE}
  {$IFDEF SIMBA_MAX_OPTIMIZATION} // O4 can do "unsafe" optimizations so this is not globally enabled.
    {$OPTIMIZATION LEVEL4}
  {$ENDIF}
{$ENDIF}

// Set in project options > custom options.. "-CfSSE"
//{$IF Defined(CPU386)}
//  {$FPUTYPE SSE3}
//{$ELSEIF Defined(CPUX86_64)}
//  {$FPUTYPE SSE64}
//{$ENDIF}

{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH TYPEHELPERS}
{$MODESWITCH ARRAYOPERATORS}
{$MODESWITCH NESTEDPROCVARS}

{$DEFINE SIMBA_MAJOR   := 2000} // this should be 980 even if SimbaVersion is 981, etc
{$DEFINE SIMBA_VERSION := 2000}

{$DEFINE SIMBA_GITHUB_URL    := 'https://github.com/Villavu/Simba'}
{$DEFINE SIMBA_BUGS_URL      := 'https://github.com/Villavu/Simba/issues'}
{$DEFINE SIMBA_DOCS_URL      := 'https://villavu.github.io/Simba/'}
{$DEFINE SIMBA_PACKAGES_URL  := 'https://raw.githubusercontent.com/Villavu/Simba-Package-List/main/list'}

{$IFDEF DARWIN}
  {$DEFINE SIMBA_ANALYTICS_URL := 'https://olly.best/simba.php'}
{$ELSE}
  {$DEFINE SIMBA_ANALYTICS_URL := 'http://olly.best/simba.php'}
{$ENDIF}

// GITHUB_XXX env variables are available when building in github actions
{$DEFINE SIMBA_COMMIT     := {$INCLUDE %GITHUB_SHORT_SHA%}}
{$DEFINE SIMBA_COMMIT_URL := {$INCLUDE %GITHUB_SERVER_URL%} + '/' + {$INCLUDE %GITHUB_REPOSITORY%} + '/commit/' + {$INCLUDE %GITHUB_SHORT_SHA%}}

{$IFDEF Lape_CDECL}
  {$DEFINE LAPE_WRAPPER_CALLING_CONV := cdecl;}
{$ENDIF}

{.$DEFINE PARSER_BENCHMARK}
{.$DEFINE PARSER_CACHE_DEBUG}
{.$DEFINE PARSER_LEAK_CHECKS}

