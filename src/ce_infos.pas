unit ce_infos;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Menus,ce_widget, ce_common, ce_sharedres;

type

  TToolInfoKind = (tikRunning, tikFindable, tikOptional);

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
    constructor Construct(TheOwner: TComponent; kind: TToolInfoKind;
      const toolName, description: string);
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

constructor TToolInfo.Construct(TheOwner: TComponent; kind: TToolInfoKind;
    const toolName, description: string);
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
  fLabel.Hint:= description;
  fLabel.ShowHint:=true;
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
  fStatus.Hint:=description;
  fStatus.ShowHint:=true;
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
begin
  if (fLabel = nil) or (fStatus = nil) then exit;
  //
  fLabel.Caption:= fToolName;
  case fKind of
    tikFindable:
    begin
      pth := exeFullName(fToolName + exeExt);
      if pth = '' then
      begin
        fStatus.Caption:= ' the tool cannot be found';
        AssignPng(fIco, 'bullet_red');
      end
      else
      begin
        fStatus.Caption:= ' the tool is available';
        AssignPng(fIco, 'bullet_green');
      end;
    end;
    tikOptional:
    begin
      pth := exeFullName(fToolName + exeExt);
      if pth = '' then
      begin
        fStatus.Caption:= ' the tool cannot be found';
        AssignPng(fIco, 'bullet_yellow');
      end
      else
      begin
        fStatus.Caption:= ' the tool is available';
        AssignPng(fIco, 'bullet_green');
      end;
    end;
    tikRunning:
    begin
      pth := exeFullName(fToolName + exeExt);
      if pth = '' then
      begin
        fStatus.Caption:= ' the tool cannot be found';
        AssignPng(fIco, 'bullet_red');
      end
      else if AppIsRunning(fToolName + exeExt) then
      begin
        fStatus.Caption:= ' the tool is available and running';
        AssignPng(fIco, 'bullet_green');
      end
      else
      begin
        fStatus.Caption:= ' the tool is available but is not running';
        AssignPng(fIco, 'bullet_yellow');
      end;
    end;
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
  toolItem := TToolInfo.Construct(self, tikOptional, 'dfmt',
    'optional, the D source code formater, needed by the Dfmt commander widget');
  toolItem.Parent := boxTools;
  toolItem.ReAlign;
  toolItem := TToolInfo.Construct(self, tikOptional, 'dfmt',
    'optional, the GDC D compiler');
  toolItem.Parent := boxTools;
  toolItem.ReAlign;
  toolItem := TToolInfo.Construct(self, tikOptional, 'ldc2',
    'optional, the LDC D compiler');
  toolItem.Parent := boxTools;
  toolItem.ReAlign;
  toolItem := TToolInfo.Construct(self, tikOptional, 'ddemangle',
    'optional, allows to demangle cryptic symbols in the message widget');
  toolItem.Parent := boxTools;
  toolItem.ReAlign;
  toolItem := TToolInfo.Construct(self, tikRunning, 'dcd-server',
    'mandatory, provides IDE-level features such as the completion');
  toolItem.Parent := boxTools;
  toolItem.ReAlign;
  toolItem := TToolInfo.Construct(self, tikFindable, 'dcd-client',
    'mandatory, provides IDE-level features such as the completion');
  toolItem.Parent := boxTools;
  toolItem.ReAlign;
  toolItem := TToolInfo.Construct(self, tikFindable, 'cesyms',
    'background tool that collects information for the symbol list widget');
  toolItem.Parent := boxTools;
  toolItem.ReAlign;
  toolItem := TToolInfo.Construct(self, tikFindable, 'cetodo',
    'background tool that collects information for the todo list widget');
  toolItem.Parent := boxTools;
  toolItem.ReAlign;
  toolItem := TToolInfo.Construct(self, tikOptional, 'dub',
    'the D package manager, mandatory to compile project in DUB format');
  toolItem.Parent := boxTools;
  toolItem.ReAlign;
  toolItem := TToolInfo.Construct(self, tikFindable, 'dmd',
    'the reference D compiler, mandatory to compile native projects, '
    + 'to unittest and to launch runnable modules');
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

