unit simba.package_github_url;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  GITHUB_URL_REPOSITORY = 'https://github.com/%s/%s/tree/%s';                              // {Owner} {Name}
  GITHUB_URL_REPOSITORY_ISSUES = 'https://github.com/%s/%s/issues';                        // {Owner} {Name}
  GITHUB_URL_DOWNLOAD_MASTER = 'https://github.com/%s/%s/archive/master.zip';              // {Owner} {Name}
  GITHUB_URL_RELEASES = 'https://api.github.com/repos/%s/%s/releases';                     // {Owner} {Name}
  GITHUB_URL_PACKAGE_OPTIONS = 'https://raw.githubusercontent.com/%s/%s/%s/.simbapackage'; // {Owner} {Name} {Version}

implementation

end.

