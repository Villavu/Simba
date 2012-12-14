unit sm_control;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,FileUtil ,sm_types,sm_web,libtar,sm_utils,sm_settings;

function GetScript(Script: TFileItem;opt: TOption): boolean;
function UpdateScript(Script: TFileItem;opt: TOption): boolean;
function RemoveScript(Script: TFileItem; opt: TOption): boolean;
implementation
function GetScript(Script: TFileItem;opt: TOption): boolean;
var
 Downloader: TDownloader;
 ScriptTar: TMemoryStream;
 TA: TTarArchive;
 DirRec : TTarDirRec;
 FS : TFileStream;
 i: integer;
begin
 result:=false;
 scriptTar:=TMemoryStream.Create;
 Downloader:=TDownloader.Create(opt.XMLSrvDesc+GetPackageUrl(Script.filename));
 try
 ScriptTar:=Downloader.GetFile(opt.XMLSrvDesc+GetPackageUrl(Script.filename));
    i:=0;
     scriptTar.Position:=0;
     TA:=TTarArchive.Create(scriptTar);
     TA.Reset;
     while TA.FindNext(DirRec) do
       begin
          if (DirRec.FileType = ftDirectory) then
            begin;
             if not DirectoryExists(opt.Simba + DirRec.Name) and not CreateDir(opt.Simba + DirRec.Name) then
            begin
        end;
     end;
     if eq(DirRec.Name, GetScriptName(script.FileName)) then
            begin
             FS := TFileStream.Create(UTF8ToSys(opt.Simba_Scripts +dirrec.name),fmCreate);
              TA.ReadFile(fs);
             FS.Free;
             result:=true;
            end;
     if (script.SubFiles.Count > 0) then
      if (i < script.SubFiles.Count) then
      begin
         if eq(DirRec.Name, script.SubFiles[i].FileName) then
           begin
             FS := TFileStream.Create(UTF8ToSys(convertPAth(script.SubFiles[i].UnpPath,opt) +dirrec.name),fmCreate);
              TA.ReadFile(fs);
             FS.Free;
             result:=true;
             inc(i);
           end;
        end;
       end;
 finally
   FreeAndNil(downloader);
   FreeAndNil(scriptTar);
 end;

end;

function UpdateScript(Script: TFileItem; opt: TOption): boolean;
begin
 result:= GetScript(Script,opt);
end;

function RemoveScript(Script: TFileItem; opt: TOption): boolean;
var
 sName: string;
 sfName: string;
 i: integer;
begin
 sName:= opt.Simba_Scripts+GetScriptName(Script.FileName);
 i:=0;
 result:=false;
  if fileexists(sName) then
   begin
    deletefile(sName);
    result:=true;
   end;
  if (Script.SubFiles.Count > 0) then
    if (i < script.SubFiles.Count) then
      begin
       sfName:=convertPAth(script.SubFiles[i].UnpPath,opt)+script.SubFiles[i].FileName;
        if fileexists(sfName) then
          deletefile(sfName);
        inc(i);
      end;
end;

end.

