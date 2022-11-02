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
{$DEFINE SIMBA_ANALYTICS_URL := 'http://olly.best/simba.php'}

// GITHUB_XXX env variables are available when building in github actions
{$DEFINE SIMBA_COMMIT     := {$INCLUDE %GITHUB_SHORT_SHA%}}
{$DEFINE SIMBA_COMMIT_URL := {$INCLUDE %GITHUB_SERVER_URL%} + '/' + {$INCLUDE %GITHUB_REPOSITORY%} + '/commit/' + {$INCLUDE %GITHUB_SHORT_SHA%}}

{$IFDEF Lape_CDECL}
  {$DEFINE LAPE_WRAPPER_CALLING_CONV := cdecl;}
{$ENDIF}

{.$DEFINE SIMBA_WINDOWS_DARKTHEME} // incomplete

