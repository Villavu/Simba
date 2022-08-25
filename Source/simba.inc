{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
{$mode ObjFPC}{$H+}

{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH TYPEHELPERS}
{$MODESWITCH ARRAYOPERATORS}
{$MODESWITCH NESTEDPROCVARS}

{$DEFINE SIMBA_MAJOR   := 1500} // this should be 980 even if SimbaVersion is 981, etc
{$DEFINE SIMBA_VERSION := 1500}

{$DEFINE SIMBA_GITHUB_URL    := 'https://github.com/Villavu/Simba'}
{$DEFINE SIMBA_BUGS_URL      := 'https://github.com/Villavu/Simba/issues'}
{$DEFINE SIMBA_DOCS_URL      := 'https://villavu.github.io/Simba/'}
{$DEFINE SIMBA_PACKAGES_URL  := 'https://raw.githubusercontent.com/Villavu/Simba-Package-List/main/list'}
{$DEFINE SIMBA_ANALYTICS_URL := 'https://olly.best/simba.php'}

// GITHUB_XXX env variables are available when building in github actions
{$DEFINE SIMBA_COMMIT     := {$include %GITHUB_SHORT_SHA%}}
{$DEFINE SIMBA_COMMIT_URL := {$include %GITHUB_SERVER_URL%} + '/' + {$include %GITHUB_REPOSITORY%} + '/commit/' + {$include %GITHUB_SHORT_SHA%}}

{$IFOPT D-}
  {$IFDEF SIMBA_O4}
    {$OPTIMIZATION LEVEL4}
  {$ENDIF}
{$ENDIF}

{$IF DEFINED(WINDOWS) AND DEFINED(CPU32)}
  {$DEFINE SIMBA_WIN32}
{$ELSEIF DEFINED(WINDOWS) AND DEFINED(CPU64)}
  {$DEFINE SIMBA_WIN64}
{$ELSEIF DEFINED(DARWIN) AND DEFINED(CPU64)}
  {$DEFINE SIMBA_DARWIN64}
{$ELSEIF DEFINED(LINUX) AND DEFINED(CPUAARCH64)}
  {$DEFINE SIMBA_AARCH64}
{$ELSEIF DEFINED(LINUX) AND DEFINED(CPU64)}
  {$DEFINE SIMBA_LINUX64}
{$ENDIF}

