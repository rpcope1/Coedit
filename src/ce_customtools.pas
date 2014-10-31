unit ce_customtools;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, process, ce_common, ce_writableComponent;

type

  TCEToolItem = class(TCollectionItem)
  private
    fExecutable: string;
    fWorkingDir: string;
    fShowWin: TShowWindowOptions;
    fOpts: TProcessOptions;
    fParameters: TStringList;
    fToolAlias: string;
    fShortcut: string;
    procedure setParameters(const aValue: TStringList);
  published
    property toolAlias: string read fToolAlias write fToolAlias;
    property options: TProcessOptions read fOpts write fOpts;
    property executable: string read fExecutable write fExecutable;
    property workingDirectory: string read fWorkingDir write fWorkingDir;
    property parameters: TStringList read fParameters write setParameters;
    property showWindows: TShowWindowOptions read fShowWin write fShowWin;
    //property shortcut: string read fShortcut write fShortcut;
  public
    constructor create(ACollection: TCollection); override;
    destructor destroy; override;
    //
    procedure execute;
  end;

  TCETools = class(TWritableComponent)
  private
    fTools: TCollection;
    function getTool(index: Integer): TCEToolItem;
    procedure setTools(const aValue: TCollection);
  published
    property tools: TCollection read fTools write setTools;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    function addTool: TCEToolItem;
    property tool[index: integer]: TCEToolItem read getTool;
  end;

implementation

uses
  ce_main;

constructor TCEToolItem.create(ACollection: TCollection);
begin
  inherited;
  fToolAlias := format('<tool %d>', [ID]);
  fParameters := TStringList.create;
end;

destructor TCEToolItem.destroy;
begin
  fParameters.Free;
  inherited;
end;

procedure TCEToolItem.setParameters(const aValue: TStringList);
begin
  fParameters.Assign(aValue);
end;

procedure TCEToolItem.execute;
var
  i: Integer;
  proc: TProcess;
begin
  proc := TProcess.Create(nil);
  try
    proc.Options := fOpts;
    if fExecutable <> '' then
      proc.Executable := CEMainForm.expandSymbolicString(fExecutable);
    proc.ShowWindow := fShowWin;
    if fWorkingDir <> '' then
      proc.CurrentDirectory := CEMainForm.expandSymbolicString(fWorkingDir);
    proc.Parameters.Clear;
    for i:= 0 to fParameters.Count-1 do
      if fParameters.Strings[i] <> '' then
        proc.Parameters.Add(CEMainForm.expandSymbolicString(fParameters.Strings[i]));
    proc.Options := proc.Options - [poUsePipes, poWaitOnExit];
    proc.Execute;
  finally
    proc.Free;
  end;
end;

constructor TCETools.create(aOwner: TComponent);
begin
  inherited;
  fTools := TCollection.Create(TCEToolItem);
end;

destructor TCETools.destroy;
begin
  fTools.Free;
  inherited;
end;

procedure TCETools.setTools(const aValue: TCollection);
begin
  fTools.Assign(aValue);
end;

function TCETools.getTool(index: Integer): TCEToolItem;
begin
  result := TCEToolItem(fTools.Items[index]);
end;

function TCETools.addTool: TCEToolItem;
begin
  result := TCEToolItem(fTools.Add);
end;

initialization
  RegisterClasses([TCEToolItem, TCETools]);
end.
