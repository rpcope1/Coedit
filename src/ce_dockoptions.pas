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
  TEditableAnchorDockOptions = class(TAnchorDockOptionsFrame, ICEEditableOptions)
  private
    fBackup: TXMLConfigStorage;
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(anEvent: TOptionEditorEvent);
    procedure doChanged(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

var
  DockOptionsEditor: TEditableAnchorDockOptions;

constructor TEditableAnchorDockOptions.Create(TheOwner: TComponent);
begin
  inherited;
  fBackup := TXMLConfigStorage.Create('',false);
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

destructor TEditableAnchorDockOptions.Destroy;
begin
  fBackup.Free;
  inherited;
end;

function TEditableAnchorDockOptions.optionedWantCategory(): string;
begin
  exit('Docking')
end;

function TEditableAnchorDockOptions.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekForm);
end;

function TEditableAnchorDockOptions.optionedWantContainer: TPersistent;
begin
  fBackup.Clear;
  DockMaster.SaveSettingsToConfig(fBackup);
  LoadFromMaster;
  exit(self);
end;

procedure TEditableAnchorDockOptions.optionedEvent(anEvent: TOptionEditorEvent);
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

procedure TEditableAnchorDockOptions.doChanged(Sender: TObject);
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
  DockOptionsEditor := TEditableAnchorDockOptions.create(nil);
finalization
  DockOptionsEditor.free;
end.

