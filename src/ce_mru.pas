unit ce_mru;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, ce_interfaces, ce_observer,
  ce_nativeproject, ce_synmemo, ce_common;

type

  (**
   * 'Most Recently Used' list for strings.
   *)
  TCEMruList = class(TStringList)
  private
    fMaxCount: Integer;
    fObj: TObject;
  protected
    fChecking: boolean;
    procedure clearOutOfRange;
    procedure setMaxCount(aValue: Integer);
    function checkItem(const S: string): boolean; virtual;
    procedure Put(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string); override;
  published
    property maxCount: Integer read fMaxCount write setMaxCount;
  public
    constructor Create; virtual;
    procedure Insert(Index: Integer; const S: string); override;
    property objectTag: TObject read fObj write fObj;
  end;

  (**
   * MRU list for filenames.
   *)
  TCEMRUFileList = class(TCEMruList)
  protected
    function checkItem(const S: string): boolean; override;
  public
    procedure assign(src: TPersistent); override;
  end;

  (**
   * MRU list for D/text files.
   * Insertion is automatic (ICEMultiDocObserver).
   *)
  TCEMRUDocumentList = class(TCEMRUFileList, ICEMultiDocObserver)
  private
    procedure docNew(aDoc: TCESynMemo);
    procedure docFocused(aDoc: TCESynMemo);
    procedure docChanged(aDoc: TCESynMemo);
    procedure docClosing(aDoc: TCESynMemo);
  public
    constructor create; override;
    destructor destroy; override;
  end;

  (**
   * MRU list for the ceodit projects.
   * Insertion is automatic (ICEProjectObserver).
   *)
  TCEMRUProjectList = class(TCEMRUFileList, ICEProjectObserver)
  private
    procedure projNew(aProject: ICECommonProject);
    procedure projChanged(aProject: ICECommonProject);
    procedure projClosing(aProject: ICECommonProject);
    procedure projFocused(aProject: ICECommonProject);
    procedure projCompiling(aProject: ICECommonProject);
  public
    constructor create; override;
    destructor destroy; override;
  end;

implementation

constructor TCEMruList.Create;
begin
  fMaxCount := 10;
end;

procedure TCEMruList.clearOutOfRange;
begin
  while Count > fMaxCount do
    delete(Count-1);
end;

procedure TCEMruList.setMaxCount(aValue: Integer);
begin
  if aValue < 0 then
    aValue := 0;
  if fMaxCount = aValue then
    exit;
  fMaxCount := aValue;
  clearOutOfRange;
end;

function TCEMruList.checkItem(const S: string): boolean;
var
  i: NativeInt;
begin
  i := indexOf(S);
  if i = -1 then
    exit(true);
  if i = 0 then
    exit(false);
  if Count < 2 then
    exit(false);
  exchange(i, i-1);
  exit( false);
end;

procedure TCEMruList.Put(Index: Integer; const S: string);
begin
  if not (checkItem(S)) then
    exit;
  inherited;
  clearOutOfRange;
end;

procedure TCEMruList.InsertItem(Index: Integer; const S: string);
begin
  if not (checkItem(S)) then
    exit;
  inherited;
  clearOutOfRange;
end;

procedure TCEMruList.Insert(Index: Integer; const S: string);
begin
  if not (checkItem(S)) then
    exit;
  inherited;
  clearOutOfRange;
end;

procedure TCEMRUFileList.assign(src: TPersistent);
var
  i: Integer;
begin
  inherited;
  for i := Count-1 downto 0 do
    if not Strings[i].fileExists then
      Delete(i);
end;

function TCEMRUFileList.checkItem(const S: string): boolean;
begin
  exit( inherited checkItem(S) and S.fileExists);
end;

constructor TCEMRUDocumentList.create;
begin
  inherited;
  EntitiesConnector.addObserver(self);
end;

destructor TCEMRUDocumentList.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TCEMRUDocumentList.docNew(aDoc: TCESynMemo);
begin
end;

procedure TCEMRUDocumentList.docFocused(aDoc: TCESynMemo);
begin
end;

procedure TCEMRUDocumentList.docChanged(aDoc: TCESynMemo);
begin
end;

procedure TCEMRUDocumentList.docClosing(aDoc: TCESynMemo);
begin
  if aDoc.fileName.fileExists and not aDoc.isTemporary then
    Insert(0, aDoc.fileName);
end;

constructor TCEMRUProjectList.create;
begin
  inherited;
  EntitiesConnector.addObserver(self);
end;

destructor TCEMRUProjectList.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TCEMRUProjectList.projNew(aProject: ICECommonProject);
begin
end;

procedure TCEMRUProjectList.projFocused(aProject: ICECommonProject);
begin
end;

procedure TCEMRUProjectList.projChanged(aProject: ICECommonProject);
begin
end;

procedure TCEMRUProjectList.projCompiling(aProject: ICECommonProject);
begin
end;

procedure TCEMRUProjectList.projClosing(aProject: ICECommonProject);
var
  fname: string;
begin
  if aProject = nil then exit;
  //
  fname := aProject.filename;
  if fname.fileExists then
    Insert(0, fname);
end;

initialization
  RegisterClasses([TCEMRUList, TCEMRUFileList, TCEMRUProjectList, TCEMRUDocumentList]);
end.
