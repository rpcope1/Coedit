unit ce_options;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, ce_common, ce_writableComponent, ce_observer;

type
  TCEOptions = class(TWritableLfmTextComponent)
  private
    fSubjPersObservers: TCECustomSubject;
  protected
    procedure defineProperties(Filer: TFiler); override;
    procedure beforeLoad; override;
    procedure beforeSave; override;
    procedure afterLoad; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  ce_interfaces;

constructor TCEOptions.Create(aOwner: TComponent);
begin
  inherited;
  fSubjPersObservers := TCESessionOptionsSubject.Create;
  //
  EntitiesConnector.addSubject(fSubjPersObservers);
end;

destructor TCEOptions.Destroy;
begin
  EntitiesConnector.removeSubject(fSubjPersObservers);
  EntitiesConnector.endUpdate;
  //
  fSubjPersObservers.Free;
  inherited;
end;

procedure TCEOptions.defineProperties(Filer: TFiler);
begin
  subjSesOptsDeclareProperties(TCESessionOptionsSubject(fSubjPersObservers), Filer);
end;

procedure TCEOptions.beforeLoad;
begin
  // ensure that the observers will be know:
  // during the app init a bulk update operation is happening,
  // cf. ce_observer.pas, initialization section.
  if EntitiesConnector.isUpdating then
    EntitiesConnector.forceUpdate;
  inherited;
end;

procedure TCEOptions.beforeSave;
begin
  subjSesOptsBeforeSave(TCESessionOptionsSubject(fSubjPersObservers));
end;

procedure TCEOptions.afterLoad;
begin
  subjSesOptsAfterLoad(TCESessionOptionsSubject(fSubjPersObservers));
end;

end.
