unit ce_dockoptions;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, AnchorDocking, AnchorDockOptionsDlg, XMLPropStorage, AnchorDockStr,
  ce_observer, ce_interfaces;

type

  (**
   * The option frame to edit the docking option, displayed in the option form.
   *)
  TDockOptionsEditor = class(TAnchorDockOptionsFrame, ICEEditableOptions)
  private
    fBackup: TXMLConfigStorage;
    //
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(anEvent: TOptionEditorEvent);
    //
    procedure doChanged(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

var
  DockOptionsEditor: TDockOptionsEditor;

constructor TDockOptionsEditor.Create(TheOwner: TComponent);
begin
  inherited;
  fBackup := TXMLConfigStorage.Create('', false);
  Master := AnchorDocking.DockMaster;
  //
  HeaderAlignLeftTrackBar.OnChange := @doChanged;
  HeaderAlignTopTrackBar.OnChange := @doChanged;
  DragThresholdTrackBar.OnChange := @doChanged;
  SplitterWidthTrackBar.OnChange := @doChanged;
  //
  FlattenHeaders.OnChange := @doChanged;
  FilledHeaders.OnChange := @doChanged;
  HideHeaderCaptionForFloatingCheckBox.OnChange := @doChanged;
  ScaleOnResizeCheckBox.OnChange := @doChanged;
  ShowHeaderCaptionCheckBox.OnChange := @doChanged;
  ShowHeaderCheckBox.OnChange := @doChanged;
  //
  HeaderStyleComboBox.OnChange:= @doChanged;
  //
  EntitiesConnector.addObserver(self);
end;

destructor TDockOptionsEditor.Destroy;
begin
  fBackup.Free;
  inherited;
end;

function TDockOptionsEditor.optionedWantCategory(): string;
begin
  exit('Docking')
end;

function TDockOptionsEditor.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekControl);
end;

function TDockOptionsEditor.optionedWantContainer: TPersistent;
begin
  fBackup.Clear;
  DockMaster.SaveSettingsToConfig(fBackup);
  LoadFromMaster;
  exit(self);
end;

procedure TDockOptionsEditor.optionedEvent(anEvent: TOptionEditorEvent);
begin
  // restores
  if anEvent = oeeCancel then
  begin
    DockMaster.LoadSettingsFromConfig(fBackup);
    LoadFromMaster;
    fBackup.Clear;
    DockMaster.SaveSettingsToConfig(fBackup);
  end;
  // accept and new backup
  if anEvent = oeeAccept then
  begin
    SaveToMaster;
    fBackup.Clear;
    DockMaster.SaveSettingsToConfig(fBackup);
  end;
end;

procedure TDockOptionsEditor.doChanged(Sender: TObject);
begin
  DragThresholdLabel.Caption:=adrsDragThreshold +
    ' ('+IntToStr(DragThresholdTrackBar.Position)+')';
  HeaderAlignTopLabel.Caption:=adrsHeaderAlignTop +
    ' ('+IntToStr(HeaderAlignTopTrackBar.Position) +')';
  HeaderAlignLeftLabel.Caption:=adrsHeaderAlignLeft +
    ' ('+IntToStr(HeaderAlignLeftTrackBar.Position) +')';
  SplitterWidthLabel.Caption:=adrsSplitterWidth +
    ' ('+IntToStr(SplitterWidthTrackBar.Position) +')';
  ShowHeaderCheckBox.Visible:=adofShow_ShowHeader in Flags;
  //
  SaveToMaster;
end;

initialization
  DockOptionsEditor := TDockOptionsEditor.create(nil);
finalization
  DockOptionsEditor.free;
end.

