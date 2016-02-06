unit ce_lcldragdrop;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, Controls, ComCtrls,
  ce_common, ce_nativeproject, ce_dubproject, ce_interfaces,
  ce_dialogs;

type

  TDDHandler = class(TObject, ICEProjectObserver)
  private
    fProj: ICECommonProject;
    procedure projNew(aProject: ICECommonProject);
    procedure projChanged(aProject: ICECommonProject);
    procedure projClosing(aProject: ICECommonProject);
    procedure projFocused(aProject: ICECommonProject);
    procedure projCompiling(aProject: ICECommonProject);
    procedure projCompiled(aProject: ICECommonProject; success: boolean);
    //
    function getFilename(src: TObject): string;
  public
    constructor create;
    destructor destroy; override;
    procedure DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DragDrop(Sender, Source: TObject; X, Y: Integer);
  end;

var
  ddHandler: TDDHandler;

implementation

uses
  ce_observer;

constructor TDDHandler.create;
begin
  EntitiesConnector.addObserver(self);
end;

destructor TDDHandler.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TDDHandler.projNew(aProject: ICECommonProject);
begin
  fProj := aProject;
end;

procedure TDDHandler.projChanged(aProject: ICECommonProject);
begin
end;

procedure TDDHandler.projClosing(aProject: ICECommonProject);
begin
  if (fProj <> nil) and (fProj = aProject) then
    fProj := nil;
end;

procedure TDDHandler.projFocused(aProject: ICECommonProject);
begin
  fProj := aProject;
end;

procedure TDDHandler.projCompiling(aProject: ICECommonProject);
begin
end;

procedure TDDHandler.projCompiled(aProject: ICECommonProject; success: boolean);
begin
end;

function TDDHandler.getFilename(src: TObject): string;
var
  lst: TListView;
  trv: TTreeView;
begin
  result := '';
  if src.isNil then exit;
  // from mini-explorer
  if src is TListView then
  begin
    lst := TListView(src);
    if lst.Selected.isNotNil and lst.Selected.Data.isNotNil then
      result := PString(lst.Selected.Data)^;
  end
  // from CE/DUB project inspector
  else if src is TTreeView then
  begin
    trv := TTreeView(src);
    if trv.Selected.isNotNil then
    begin
      result := trv.Selected.Text;
      if not result.fileExists and assigned(fProj) then
        result := fProj.filename.extractFilePath + result;
    end;
  end;
  {$IFNDEF WINDOWS}
  if (result.length > 1) and (result[2] = '/') then
    result := result[2..result.length];
  {$ENDIF}
end;

procedure TDDHandler.DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  fname: string;
begin
  fname := getFilename(Source);
  Accept := fname.fileExists and not fname.dirExists;
end;

procedure TDDHandler.DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  fname: string;
  proj: boolean = false;
begin
  if Source.isNil then exit;
  fname := getFilename(Source);
  if not fname.fileExists then exit;

  if isValidNativeProject(fname) then
  begin
    if assigned(fProj) then
    begin
      if fProj.modified and (dlgFileChangeClose(fProj.filename) = mrCancel) then
        exit;
      fProj.getProject.Free;
    end;
    TCENativeProject.create(nil);
    proj := true;
  end
  else if isValidDubProject(fname) then
  begin
    if assigned(fProj) then
    begin
      if fProj.modified and (dlgFileChangeClose(fProj.filename) = mrCancel) then
        exit;
      fProj.getProject.Free;
    end;
    TCEDubProject.create(nil);
    proj := true;
  end;
  if assigned(fProj) and proj then
    fProj.loadFromFile(fname)
  else
    getMultiDocHandler.openDocument(fname);
end;

initialization
  ddHandler:= TDDHandler.create;

finalization
  ddHandler.free;

end.

