unit ce_libman;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, ce_common, ce_writableComponent, ce_dcd, LazFileUtils,
  ce_dialogs;

type

  (**
   * Represents a D static library. In a project libAlias allows to
   * resolve automatically the dependencies of a project.
   *)
  TLibraryItem = class(TCollectionItem)
  private
    fAlias: string;
    fSourcePath: string;
    fLibFile: string;
    fProjFile: string;
  published
    property libAlias: string read fAlias write fAlias;
    property libSourcePath: string read fSourcePath write fSourcePath;
    property libFile: string read fLibFile write fLibFile;
    property projectFile: string read fProjFile write fProjFile;
  end;

  (**
   * Represents all the D libraries present on this system.
   *)
  TLibraryManager = class(TWritableLfmTextComponent)
  private
    fCol: TCollection;
    procedure setCol(const aValue: TCollection);
  published
    property libraries: TCollection read fCol write setCol;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure getLibFiles(someAliases, aList: TStrings);
    procedure getLibSources(someAliases, aList: TStrings);
    //
    procedure updateDCD;
  end;

const
  libFname = 'libraryManager.txt';

var
  LibMan: TLibraryManager;

implementation

constructor TLibraryManager.create(aOwner: TComponent);
var
  fName: string;
  {$IFDEF WINDOWS}
  fDmdPath: string;
  {$ENDIF}
begin
  inherited;
  fCol := TCollection.Create(TLibraryItem);
  fname := getCoeditDocPath + libFname;
  if fname.fileExists then
    loadFromFile(fname);
  if fCol.Count = 0 then
  begin
    {$IFDEF WINDOWS}
    fDmdPath := ExeSearch('dmd.exe');
    if fDmdPath.fileExists then
    begin
      // add phobos
      fname := fDmdPath.ExtractFileDir;
      fname := fname.ExtractFileDir;
      with TLibraryItem(fCol.Add) do begin
        libAlias := 'phobos';
        libFile  := fname + '\lib\phobos.lib';
        libSourcePath := fname.ExtractFileDir + '\src\phobos';
      end;
      // add druntime (no lib - only for DCD)
      fname := fDmdPath.ExtractFileDir;
      fname := fname.ExtractFileDir;
      with TLibraryItem(fCol.Add) do begin
        libAlias := 'druntime';
        libFile  := '';
        libSourcePath := fname.ExtractFileDir + '\src\druntime\import';
      end;
    end;
    {$ENDIF}
    {$IFDEF LINUX}
    // add phobos
    if '/usr/include/dmd/phobos'.dirExists then
    begin
      with TLibraryItem(fCol.Add) do begin
        libAlias := 'phobos';
        libFile := '';
        libSourcePath := '/usr/include/dmd/phobos';
      end;
    end;
    // add druntime (no lib - only for DCD)
    if '/usr/include/dmd/druntime/import'.dirExists then
    begin
      with TLibraryItem(fCol.Add) do begin
        libAlias := 'druntime';
        libFile  := '';
        libSourcePath := '/usr/include/dmd/druntime/import';
      end;
    end;
    {$ENDIF}
    {$IFDEF DARWIN}
    assert(false, 'to be implemented');
    {$ENDIF}
  end;
  if fCol.Count = 0 then
  begin
    dlgOkError(
      'Coedit failed to automatically add "druntime" and "phobos" to the library manager.'
    + 'These two items have to be added manually following the procedure described in the wiki.'
    );
  end;
  updateDCD;
end;

destructor TLibraryManager.destroy;
begin
  ForceDirectoriesUTF8(getCoeditDocPath);
  LibMan.saveToFile(getCoeditDocPath + libFname);
  fCol.Free;
  inherited;
end;

procedure TLibraryManager.setCol(const aValue: TCollection);
begin
  fCol.assign(aValue);
end;

procedure TLibraryManager.updateDCD;
var
  itm: TLibraryItem;
  str: TStringList;
  i: Integer;
begin
  if not DcdWrapper.available then exit;
  // note: new items are directly handled but removed ones still in cache until server restarts.
  str := TStringList.Create;
  try
    for i := 0 to fCol.Count-1 do
    begin
      itm := TLibraryItem(fCol.Items[i]);
      str.Add(itm.libSourcePath);
    end;
    DcdWrapper.addImportFolders(str);
  finally
    str.Free;
  end;
end;

(**
 * the caller gets all the *.lib/*.a files in aList if someAliases is nil
 * otherwise the static libs selected by the aliases in someAliases.
 *)
procedure TLibraryManager.getLibFiles(someAliases, aList: TStrings);
var
  itm: TLibraryItem;
  lst: TStringList;
  i,j: Integer;
  dir: string;
begin
  for i := 0 to fCol.Count-1 do
  begin
    itm := TLibraryItem(fCol.Items[i]);
    if someAliases.isNotNil then
      if someAliases.IndexOf(itm.libAlias) = -1 then
        continue;
    // single lib files
    if fileExists(itm.libFile) then
    begin
      if aList.IndexOf(itm.libFile) <> -1 then
        continue;
      aList.Add(itm.libFile);
    end
    // folder of lib file
    else if itm.libFile.dirExists then
    begin
      lst := TStringList.Create;
      try
        dir := itm.libFile;
        if itm.libFile[dir.length] = DirectorySeparator then
          dir := dir[1..dir.length-1];
        listFiles(lst, dir);
        for j:= 0 to lst.Count-1 do
        begin
          if lst[j].extractFileExt = libExt then
            if aList.IndexOf(lst[j]) = -1 then
              aList.Add(lst[j]);
        end;
      finally
        lst.Free;
      end;
    end;
  end;
end;

(**
 * the caller gets all the paths were are located the lib sources in aList if someAliases is nil
 * otherwise the paths where are located the lib sources selected by the aliases in someAliases.
 *)
procedure TLibraryManager.getLibSources(someAliases, aList: TStrings);
var
  itm: TLibraryItem;
  i: Integer;
begin
  for i := 0 to fCol.Count-1 do
  begin
    itm := TLibraryItem(fCol.Items[i]);
    if someAliases.isNotNil then
      if someAliases.IndexOf(itm.libAlias) = -1 then
        continue;
    //
    if aList.IndexOf(itm.libSourcePath) <> -1 then
      continue;
    if not itm.libSourcePath.dirExists then
      continue;
    aList.Add('-I' + itm.libSourcePath);
  end;
end;

initialization
  registerClasses([TLibraryManager, TLibraryItem]);
  LibMan := TLibraryManager.create(nil);
finalization
  LibMan.Free;
end.
