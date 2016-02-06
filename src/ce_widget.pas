unit ce_widget;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ActnList, Menus,
  xAnchorDocking, ce_interfaces;

type

  (**
   * Base type for an UI module.
   *)
  PTCEWidget = ^TCEWidget;
  TCEWidget = class(TForm, ICEContextualActions)
    Content: TPanel;
    Back: TPanel;
    contextMenu: TPopupMenu;
  private
    fUpdating: boolean;
    fDelayDur: Integer;
    fLoopInter: Integer;
    fUpdaterAuto: TTimer;
    fUpdaterDelay: TTimer;
    fImperativeUpdateCount: Integer;
    fLoopUpdateCount: Integer;
    procedure setDelayDur(aValue: Integer);
    procedure setLoopInt(aValue: Integer);
    procedure updaterAutoProc(Sender: TObject);
    procedure updaterLatchProc(Sender: TObject);
  protected
    fIsDockable: boolean;
    fIsModal: boolean;
    fID: string;
    // a descendant overrides to implement a periodic update.
    procedure updateLoop; virtual;
    // a descendant overrides to implement an imperative update.
    procedure updateImperative; virtual;
    // a descendant overrides to implement a delayed update.
    procedure updateDelayed; virtual;
    //
    function contextName: string; virtual;
    function contextActionCount: integer; virtual;
    function contextAction(index: integer): TAction; virtual;
    //
    function getIfModal: boolean;
  published
    property updaterByLoopInterval: Integer read fLoopInter write setLoopInt;
    property updaterByDelayDuration: Integer read fDelayDur write setDelayDur;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    // prevent closing when 'locked' is cjecked in the header context menu
    function closeQuery: boolean; override;
    // restarts the wait period to the delayed update event.
    // if not re-called during 'updaterByDelayDuration' ms then
    // 'UpdateByDelay' is called once.
    procedure beginDelayedUpdate;
    // prevent any pending update.
    procedure stopDelayedUpdate;
    // calls immediattly any pending delayed update.
    procedure forceDelayedUpdate;

    // increments the imperative updates count.
    procedure beginImperativeUpdate;
    // decrements the imperative updates count and call updateImperative() if the
    // counter value is equal to zero.
    procedure endImperativeUpdate;
    // calls updateImperative() immediatly
    procedure forceImperativeUpdate;

    // increment a flag used to indicate if updateLoop has to be called
    procedure IncLoopUpdate;

    procedure showWidget;

    // returns true if one of the three updater is processing.
    property updating: boolean read fUpdating;
    // true by default, allow a widget to be docked.
    property isDockable: boolean read fIsDockable;
    // not if isDockable, otherwise a the widget is shown as modal form.
    property isModal: boolean read getIfModal;
  end;

  (**
   * TCEWidget list.
   *)
  TCEWidgetList = class(TFPList)
  private
    function getWidget(index: integer): TCEWidget;
  public
    procedure addWidget(aValue: PTCEWidget);
    property widget[index: integer]: TCEWidget read getWidget;
  end;

  TWidgetEnumerator = class
    fList: TCEWidgetList;
    fIndex: Integer;
    function getCurrent: TCEWidget;
    Function moveNext: boolean;
    property current: TCEWidget read getCurrent;
  end;

  operator enumerator(aWidgetList: TCEWidgetList): TWidgetEnumerator;

   function CompareWidgCaption(Item1, Item2: Pointer): Integer;

implementation
{$R *.lfm}

uses
  ce_observer;

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEWidget.create(aOwner: TComponent);
var
  i: Integer;
  itm: TmenuItem;
begin
  inherited;
  fIsDockable := true;
  fUpdaterAuto := TTimer.Create(self);
  fUpdaterAuto.Interval := 70;
  fUpdaterAuto.OnTimer := @updaterAutoProc;
  fUpdaterDelay := TTimer.Create(self);

  updaterByLoopInterval := 70;
  updaterByDelayDuration := 500;

  for i := 0 to contextActionCount-1 do
  begin
    itm := TMenuItem.Create(self);
    itm.Action := contextAction(i);
    contextMenu.Items.Add(itm);
  end;
  PopupMenu := contextMenu;

  EntitiesConnector.addObserver(self);
end;

destructor TCEWidget.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

function TCEWidget.closeQuery: boolean;
begin
  result := inherited;
  if fIsDockable and (not DockMaster.AllowDragging) and not
    (DockMaster.GetAnchorSite(self).GetTopParent = DockMaster.GetAnchorSite(self)) then
      result := false;
end;

function TCEWidget.getIfModal: boolean;
begin
  if isDockable then result := false
  else result := fIsModal;
end;

procedure TCEWidget.showWidget;
var
  win: TControl;
begin
  if isDockable then
  begin
    win := DockMaster.GetAnchorSite(self);
    if win <> nil then
    begin
      win.Show;
      win.BringToFront;
    end;
  end
  else begin
    if isModal then ShowModal else
    begin
      Show;
      BringToFront;
    end;
  end;
end;
{$ENDREGION}

{$REGION ICEContextualActions---------------------------------------------------}
function TCEWidget.contextName: string;
begin
  result := '';
end;

function TCEWidget.contextActionCount: integer;
begin
  result := 0;
end;

function TCEWidget.contextAction(index: integer): TAction;
begin
  result := nil;
end;
{$ENDREGION}

{$REGION Updaters---------------------------------------------------------------}
procedure TCEWidget.setDelayDur(aValue: Integer);
begin
  if aValue < 100 then aValue := 100;
  if fDelayDur = aValue then exit;
  fDelayDur := aValue;
  fUpdaterDelay.Interval := fDelayDur;
end;

procedure TCEWidget.setLoopInt(aValue: Integer);
begin
  if aValue < 30 then aValue := 30;
  if fLoopInter = aValue then exit;
  fLoopInter := aValue;
  fUpdaterAuto.Interval := fLoopInter;
end;

procedure TCEWidget.IncLoopUpdate;
begin
  inc(fLoopUpdateCount);
end;

procedure TCEWidget.beginImperativeUpdate;
begin
  Inc(fImperativeUpdateCount);
end;

procedure TCEWidget.endImperativeUpdate;
begin
  Dec(fImperativeUpdateCount);
  if fImperativeUpdateCount > 0 then exit;
  fUpdating := true;
  updateImperative;
  fUpdating := false;
  fImperativeUpdateCount := 0;
end;

procedure TCEWidget.forceImperativeUpdate;
begin
  fUpdating := true;
  updateImperative;
  fUpdating := false;
  fImperativeUpdateCount := 0;
end;

procedure TCEWidget.beginDelayedUpdate;
begin
  fUpdaterDelay.Enabled := false;
  fUpdaterDelay.Enabled := true;
  fUpdaterDelay.OnTimer := @updaterLatchProc;
end;

procedure TCEWidget.stopDelayedUpdate;
begin
  fUpdaterDelay.OnTimer := nil;
end;

procedure TCEWidget.forceDelayedUpdate;
begin
  updaterLatchProc(nil);
end;

procedure TCEWidget.updaterAutoProc(Sender: TObject);
begin
  fUpdating := true;
  if fLoopUpdateCount > 0 then
  	updateLoop;
  fLoopUpdateCount := 0;
  fUpdating := false;
end;

procedure TCEWidget.updaterLatchProc(Sender: TObject);
begin
  fUpdating := true;
  updateDelayed;
  fUpdating := false;
  fUpdaterDelay.OnTimer := nil;
end;

procedure TCEWidget.updateLoop;
begin
end;

procedure TCEWidget.updateImperative;
begin
end;

procedure TCEWidget.updateDelayed;
begin
end;
{$ENDREGION}

{$REGION TCEWidgetList----------------------------------------------------------}
function CompareWidgCaption(Item1, Item2: Pointer): Integer;
type
  PWidg = ^TCEWidget;
begin
  result := AnsiCompareStr(PWidg(Item1)^.Caption, PWidg(Item2)^.Caption);
end;

function TCEWidgetList.getWidget(index: integer): TCEWidget;
begin
  result := PTCEWidget(Items[index])^;
end;

procedure TCEWidgetList.addWidget(aValue: PTCEWidget);
begin
  add(Pointer(aValue));
end;

function TWidgetEnumerator.getCurrent:TCEWidget;
begin
  result := fList.widget[fIndex];
end;

function TWidgetEnumerator.moveNext: boolean;
begin
  Inc(fIndex);
  result := fIndex < fList.Count;
end;

operator enumerator(aWidgetList: TCEWidgetList): TWidgetEnumerator;
begin
  result := TWidgetEnumerator.Create;
  result.fList := aWidgetList;
  result.fIndex := -1;
end;
{$ENDREGION}

end.
