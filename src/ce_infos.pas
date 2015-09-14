unit ce_infos;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Menus,ce_widget, ce_common;

type

  TToolInfoKind = (tikRunning, tikFindable);

  TToolInfo = class(TWinControl)
  private
    fLabel: TLabel;
    fStatus: TStaticText;
    fKind: TToolInfoKind;
    fToolName: string;
    fIco: TSpeedButton;
    procedure buttonClick(sender: TObject);
  protected
    procedure SetVisible(Value: Boolean); override;
  public
    constructor Construct(TheOwner: TComponent; kind: TToolInfoKind;const toolName: string);
    procedure refreshStatus;
    procedure Update; override;
  end;


  { TCEInfoWidget }

  TCEInfoWidget = class(TCEWidget)
    boxTools: TScrollBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
  private
    procedure RefreshAllStatus;
  protected
    procedure SetVisible(Value: Boolean); override;
  public
    constructor create(aOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

constructor TToolInfo.Construct(TheOwner: TComponent; kind: TToolInfoKind;const toolName: string);
begin
  Inherited create(TheOwner);
  Align  := alTop;
  height := 26;
  width := 200;
  //
  fLabel := TLabel.Create(self);
  fLabel.AutoSize:=false;
  fLabel.Parent := self;
  fLabel.Align:= alLeft;
  fLabel.Width:= 70;
  fLabel.BorderSpacing.Around := 2;
  //
  fIco := TSpeedButton.Create(self);
  fIco.Parent := self;
  fIco.Align:= alLeft;
  fIco.Width:= 22;
  fIco.Flat:=true;
  fIco.BorderSpacing.Around := 2;
  fIco.OnClick:= @buttonClick;
  fIco.Hint:= 'refresh the status';
  fIco.ShowHint:= true;
  //
  fStatus := TStaticText.Create(self);
  fStatus.Parent:=self;
  fStatus.Align:= alClient;
  fStatus.BorderSpacing.Around := 2;
  fStatus.BorderStyle := sbsSunken;
  fStatus.AutoSize:=false;
  fStatus.Width:= 800;
  //
  fKind:=kind;
  fToolName:=toolName;
  refreshStatus;
end;

procedure TToolInfo.SetVisible(Value: Boolean);
begin
  inherited;
  refreshStatus;
end;

procedure TToolInfo.Update;
begin
  inherited;
  refreshStatus;
end;

procedure TToolInfo.buttonClick(sender: TObject);
begin
  refreshStatus;
end;

procedure TToolInfo.refreshStatus;
var
  pth: string;
  png: TPortableNetworkGraphic;
begin
  if (fLabel = nil) or (fStatus = nil) then exit;
  //
  fLabel.Caption:= fToolName;
  png := TPortableNetworkGraphic.Create;
  try case fKind of
    tikFindable:
    begin
      pth := exeFullName(fToolName + exeExt);
      if pth = '' then
      begin
        fStatus.Caption:= ' the tool cannot be found';
        png.LoadFromLazarusResource('bullet_red');
      end
      else
      begin
        fStatus.Caption:= ' the tool is available';
        png.LoadFromLazarusResource('bullet_green');
      end;
    end;
    tikRunning:
    begin
      pth := exeFullName(fToolName + exeExt);
      if pth = '' then
      begin
        fStatus.Caption:= ' the tool cannot be found';
        png.LoadFromLazarusResource('bullet_red');
      end
      else if AppIsRunning(fToolName + exeExt) then
      begin
        fStatus.Caption:= ' the tool is available and running';
        png.LoadFromLazarusResource('bullet_green');
      end
      else
      begin
        fStatus.Caption:= ' the tool is available but is not running';
        png.LoadFromLazarusResource('bullet_yellow');
      end;
    end;
  end;
  finally
    fIco.Glyph.Assign(png);
    png.Free;
  end;
  ReAlign;
  Invalidate;
end;

constructor TCEInfoWidget.create(aOwner: TComponent);
var
  toolItem: TToolInfo;
begin
  inherited;
  fIsModal := true;
  fIsDockable := false;
  //
  toolItem := TToolInfo.Construct(self, tikFindable, 'ddemangle');
  toolItem.Parent := boxTools;
  toolItem.ReAlign;
  toolItem := TToolInfo.Construct(self, tikRunning, 'dcd-server');
  toolItem.Parent := boxTools;
  toolItem.ReAlign;
  toolItem := TToolInfo.Construct(self, tikFindable, 'dcd-client');
  toolItem.Parent := boxTools;
  toolItem.ReAlign;
  toolItem := TToolInfo.Construct(self, tikFindable, 'cesyms');
  toolItem.Parent := boxTools;
  toolItem.ReAlign;
  toolItem := TToolInfo.Construct(self, tikFindable, 'cetodo');
  toolItem.Parent := boxTools;
  toolItem.ReAlign;
  toolItem := TToolInfo.Construct(self, tikFindable, 'dub');
  toolItem.Parent := boxTools;
  toolItem.ReAlign;
  toolItem := TToolInfo.Construct(self, tikFindable, 'dmd');
  toolItem.Parent := boxTools;
  toolItem.ReAlign;
  //
  Realign;
end;

procedure TCEInfoWidget.RefreshAllStatus;
var
  i: integer;
begin
  for i := 0 to boxTools.ControlCount -1 do
  begin
    if not (boxTools.Controls[i] is TToolInfo) then
      continue;
    TToolInfo(boxTools.Controls[i]).refreshStatus;
  end;
end;

procedure TCEInfoWidget.SetVisible(Value: Boolean);
begin
  inherited;
  if Visible then
    RefreshAllStatus;
end;

end.

