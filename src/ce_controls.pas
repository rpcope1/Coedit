unit ce_controls;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, buttons;

type
  TCEPageControlButton = (pbClose, pbMoveLeft, pbMoveRight, pbAdd);
  TCEPageControlButtons = set of TCEPageControlButton;

const
  CEPageControlDefaultButtons = [pbClose, pbMoveLeft, pbMoveRight, pbAdd];

type

  // Used instead of a TTabSheet since only the caption is interesting
  TCEPage = class(TCustomControl)
  private
    function getIndex: integer;
  protected
    procedure realSetText(const Value: TCaption); override;
  public
    property index: integer read getIndex;
  end;

  (**
   * Minimalist page-control dedicated to Coedit
   *
   * - get rid of the framed aspect of the default LCL one
   * - no published props, since CE has to be compilable w/o extra IDE comps
   * - add/close/move left and right speed buttons
   *)
  TCEPageControl = class(TWinControl)
  private
    fHeader: TWinControl;
    fTabs: TTabControl;
    fCloseBtn: TSpeedButton;
    fMoveLeftBtn: TSpeedButton;
    fMoveRightBtn: TSpeedButton;
    fAddBtn: TSpeedButton;
    fContent: TPanel;
    fPages: TFPList;
    fPageIndex: integer;
    fButtons: TCEPageControlButtons;
    fOnChanged: TNotifyEvent;
    fOnChanging: TTabChangingEvent;

    procedure btnCloseClick(sender: TObject);
    procedure btnMoveLeftClick(sender: TObject);
    procedure btnMoveRightClick(sender: TObject);
    procedure btnAddClick(sender: TObject);

    procedure tabsChanging(Sender: TObject; var AllowChange: Boolean);
    procedure tabsChanged(sender: TObject);
    procedure hidePage(index: integer);
    procedure showPage(index: integer);
    procedure setPageIndex(index: integer);
    procedure setButtons(value: TCEPageControlButtons);
    procedure setCurrentPage(value: TCEPage);
    function getCurrentPage: TCEPage;
    function getPageCount: integer;
    function getPage(index: integer): TCEPage;

    procedure changedNotify;
    procedure updateButtonsState;

  public
    constructor Create(aowner: TComponent); override;
    destructor Destroy; override;

    function  addPage: TCEPage;
    procedure deletePage(index: integer);
    function  getPageIndex(page: TCEPage): integer;
    procedure movePageRight;
    procedure movePageLeft;

    property currentPage: TCEPage read getCurrentPage write setCurrentPage;
    property pageIndex: integer read fPageIndex write setPageIndex;
    property pageCount: integer read getPageCount;
    property pages[index: integer]: TCEPage read getPage; default;

    property buttons: TCEPageControlButtons read fButtons write setButtons;
    property closeButton: TSpeedButton read fCloseBtn;
    property moveLeftButton: TSpeedButton read fMoveLeftBtn;
    property moveRightButton: TSpeedButton read fMoveRightBtn;
    property addButton: TSpeedButton read fAddBtn;

    property onChanged: TNotifyEvent read fOnChanged write fOnChanged;
    property onChanging: TTabChangingEvent read fOnChanging write fOnChanging;
  end;

implementation

function TCEPage.getIndex: integer;
var
  ctrl: TCEPageControl;
  i: integer;
begin
  ctrl := TCEPageControl(owner);
  for i := 0 to ctrl.pageCount-1 do
      if ctrl.pages[i] = self then
        exit(i);
  exit(-1);
end;

procedure TCEPage.RealSetText(const Value: TCaption);
var
  i: integer;
  ctrl: TCEPageControl;
begin
  inherited;
  ctrl :=  TCEPageControl(owner);
  i := ctrl.getPageIndex(self);
  if i <> -1 then ctrl.fTabs.Tabs.Strings[i] := caption;
end;

constructor TCEPageControl.Create(aowner: TComponent);
begin
  inherited;

  fHeader := TWinControl.Create(self);
  fHeader.Parent:= self;
  fHeader.Align := alTop;
  fHeader.Height:= 32;

  fTabs := TTabControl.Create(self);
  fTabs.Parent:= fHeader;
  fTabs.Align := alClient;
  fTabs.Options:=[];
  fTabs.OnChange:=@tabsChanged;
  fTabs.OnChanging:=@tabsChanging;

  fMoveLeftBtn:= TSpeedButton.Create(self);
  fMoveLeftBtn.Parent := fHeader;
  fMoveLeftBtn.Align:= alRight;
  fMoveLeftBtn.Width:= 28;
  fMoveLeftBtn.BorderSpacing.Around:= 2;
  fMoveLeftBtn.ShowCaption:=false;
  fMoveLeftBtn.OnClick:=@btnMoveLeftClick;
  fMoveLeftBtn.Hint:='move current page to the left';

  fMoveRightBtn:= TSpeedButton.Create(self);
  fMoveRightBtn.Parent := fHeader;
  fMoveRightBtn.Align:= alRight;
  fMoveRightBtn.Width:= 28;
  fMoveRightBtn.BorderSpacing.Around:= 2;
  fMoveRightBtn.ShowCaption:=false;
  fMoveRightBtn.OnClick:=@btnMoveRightClick;
  fMoveRightBtn.Hint:='move current page to the right';

  fAddBtn:= TSpeedButton.Create(self);
  fAddBtn.Parent := fHeader;
  fAddBtn.Align:= alRight;
  fAddBtn.Width:= 28;
  fAddBtn.BorderSpacing.Around:= 2;
  fAddBtn.ShowCaption:=false;
  fAddBtn.OnClick:=@btnAddClick;
  fAddBtn.Hint:='add a new page';

  fCloseBtn := TSpeedButton.Create(self);
  fCloseBtn.Parent := fHeader;
  fCloseBtn.Align:= alRight;
  fCloseBtn.Width:= 28;
  fCloseBtn.BorderSpacing.Around:= 2;
  fCloseBtn.ShowCaption:=false;
  fCloseBtn.OnClick:=@btnCloseClick;
  fCloseBtn.Hint:='close current page';

  fContent := TPanel.Create(self);
  fContent.Parent := self;
  fContent.Align  := alClient;
  fContent.BevelInner:= bvNone;
  fContent.BevelOuter:= bvNone;
  fContent.BorderStyle:=bsNone;
  fContent.BorderSpacing.Around:=0;

  fPages := TFPList.Create;
  fPageIndex := -1;

  fButtons:= CEPageControlDefaultButtons;
  updateButtonsState;
end;

destructor TCEPageControl.Destroy;
begin
  while fPages.Count > 0 do
    deletePage(fPages.Count-1);
  fPages.Free;
  inherited;
end;

procedure TCEPageControl.changedNotify;
begin
  updateButtonsState;
  if assigned(fOnChanged) then
    fOnChanged(self);
end;

procedure TCEPageControl.tabsChanged(sender: TObject);
begin
  setPageIndex(fTabs.TabIndex);
end;

procedure TCEPageControl.tabsChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if assigned(fOnChanging) then fOnChanging(self, AllowChange);
end;

procedure TCEPageControl.hidePage(index: integer);
var
  pge: TCEPage;
  ctl: TControl;
begin
  if (index < 0) or (index > fPages.Count-1) then
    exit;

  pge := TCEPage(fPages.Items[index]);
  pge.Visible:=false;
  for ctl in pge.GetEnumeratorControls do
    ctl.Visible:=false;
end;

procedure TCEPageControl.showPage(index: integer);
var
  pge: TCEPage;
  ctl: TControl;
begin
  if (index < 0) or (index > fPages.Count-1) then
    exit;

  pge := TCEPage(fPages.Items[index]);
  pge.Visible:=true;
  pge.Repaint;
  for ctl in pge.GetEnumeratorControls do
    ctl.Visible:=true;
end;

procedure TCEPageControl.setPageIndex(index: integer);
begin
  if (index > fPages.Count-1) then
    index := fPages.Count-1;
  if (index < 0) then
      exit;

  hidePage(fPageIndex);
  fPageIndex := index;
  showPage(fPageIndex);

  if fTabs.TabIndex <> fPageIndex then
    fTabs.TabIndex:= fPageIndex;

  changedNotify;
end;

function TCEPageControl.addPage: TCEPage;
var
  pge: TCEPage;
begin
  pge := TCEPage.Create(self);
  pge.Parent := fContent;
  pge.Align:= alClient;

  fPages.Add(pge);
  fTabs.Tabs.Add(format('', [fPages.Count]));
  setPageIndex(fTabs.Tabs.Count-1);

  result := pge;
end;

procedure TCEPageControl.deletePage(index: integer);
begin
  if (index > fPages.Count-1) or (index < 0) then
    exit;

  TCEPage(fPages.Items[index]).Free;
  fPages.Delete(index);
  fTabs.Tabs.Delete(index);

  if fPageIndex >= fPages.Count then
    fPageIndex -= 1;
  updateButtonsState;
  if fPages.Count = 0 then
    exit;

  setPageIndex(fPageIndex);
end;

function TCEPageControl.getPageIndex(page: TCEPage): integer;
begin
  exit(fPages.IndexOf(page));
end;

function TCEPageControl.getCurrentPage: TCEPage;
begin
  if (fPageIndex < 0) or (fPageIndex > fPages.Count-1) then
    exit(nil)
  else
    exit(TCEPage(fPages.Items[fPageIndex]));
end;

procedure TCEPageControl.setCurrentPage(value: TCEPage);
begin
  setPageIndex(getPageIndex(value));
end;

function TCEPageControl.getPageCount: integer;
begin
  exit(fPages.Count);
end;

function TCEPageControl.getPage(index: integer): TCEPage;
begin
  exit(TCEPage(fPages.Items[index]));
end;

procedure TCEPageControl.movePageRight;
begin
  if fPageIndex = fPages.Count-1 then
    exit;

  fPages.Exchange(fPageIndex, fPageIndex + 1);
  fTabs.Tabs.Exchange(fPageIndex, fPageIndex + 1);
  setPageIndex(fPageIndex+1);
end;

procedure TCEPageControl.movePageLeft;
begin
  if fPageIndex <= 0 then
    exit;

  fPages.Exchange(fPageIndex, fPageIndex - 1);
  fTabs.Tabs.Exchange(fPageIndex, fPageIndex - 1);
  setPageIndex(fPageIndex-1);
end;

procedure TCEPageControl.btnCloseClick(sender: TObject);
begin
  deletePage(fPageIndex);
end;

procedure TCEPageControl.btnMoveLeftClick(sender: TObject);
begin
  movePageLeft;
end;

procedure TCEPageControl.btnMoveRightClick(sender: TObject);
begin
  movePageRight;
end;

procedure TCEPageControl.btnAddClick(sender: TObject);
begin
  addPage;
end;

procedure TCEPageControl.setButtons(value: TCEPageControlButtons);
begin
  if fButtons = value then
    exit;

  fButtons := value;
  updateButtonsState;
  fHeader.ReAlign;
end;

procedure TCEPageControl.updateButtonsState;
begin
  fHeader.DisableAlign;
  fCloseBtn.Visible:= pbClose in fButtons;
  fMoveLeftBtn.Visible:= pbMoveLeft in fButtons;
  fCloseBtn.Visible:= pbMoveRight in fButtons;
  fAddBtn.Visible:= pbAdd in fButtons;
  fHeader.EnableAlign;
  fCloseBtn.Enabled := fPageIndex <> -1;
  fMoveLeftBtn.Enabled := fPageIndex > 0;
  fMoveRightBtn.Enabled := fPageIndex < fPages.Count-1;
end;

end.

