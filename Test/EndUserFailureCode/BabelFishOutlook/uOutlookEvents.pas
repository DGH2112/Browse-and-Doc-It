unit uOutlookEvents;

interface

uses Windows, Classes, ActiveX, SysUtils, Outlook_TLB, Office_TLB, OleServer;

type

  TOutlookServer = class(TOleServer)
  private
    FIntf : IUnknown;
    function GetConnected: boolean;
  public
    constructor Create(svrIntf: IUnknown); reintroduce; virtual;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IUnknown);
    procedure Disconnect; override;
    property Intf : IUnknown read FIntf;
    property Connected : boolean read GetConnected;
  end;

  TOutlookExplorer = class;
  TBeforeFolderSwitchEvent = procedure (Sender : TOutlookExplorer; const NewFolder: IDispatch; var Cancel: WordBool) of object;
  TBeforeViewSwitchEvent = procedure (Sender : TOutlookExplorer; const NewView: IDispatch; var Cancel: WordBool) of object;

  TOutlookExplorer = class(TOutlookServer)
  private
    FOnClose: TNotifyEvent;
    FOnBeforeFolderSwitch: TBeforeFolderSwitchEvent;
    FOnBeforeViewSwitch: TBeforeViewSwitchEvent;
    FOnSelectionChange: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnFolderSwitch: TNotifyEvent;
    FOnViewSwitch: TNotifyEvent;
    FOnActivate: TNotifyEvent;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
  public
    //need to add more events
    property OnActivate : TNotifyEvent read FOnActivate write FOnActivate;
    property OnFolderSwitch : TNotifyEvent read FOnFolderSwitch write FOnFolderSwitch;
    property OnBeforeFolderSwitch : TBeforeFolderSwitchEvent read FOnBeforeFolderSwitch write FOnBeforeFolderSwitch;
    property OnViewSwitch : TNotifyEvent read FOnViewSwitch write FOnViewSwitch;
    property OnBeforeViewSwitch : TBeforeViewSwitchEvent read FOnBeforeViewSwitch write FOnBeforeViewSwitch;
    property OnDeactivate : TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnSelectionChange : TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnClose : TNotifyEvent read FOnClose write FOnClose;
  end;

  TOutlookBarPane = class;

  TBeforeNavigateEvent = procedure (Sender : TOutlookBarPane; const Shortcut: OutlookBarShortcut; var Cancel: WordBool) of object;
  TBeforeGroupSwitchEvent = procedure (Sender : TOutlookBarPane; const ToGroup: OutlookBarGroup; var Cancel: WordBool) of object;

  TOutlookBarPane = class(TOutlookServer)
  private
    FOnBeforeGroupSwitch: TBeforeGroupSwitchEvent;
    FOnBeforeNavigate: TBeforeNavigateEvent;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
  public
    //events
    property OnBeforeNavigate : TBeforeNavigateEvent read FOnBeforeNavigate write FOnBeforeNavigate;
    property OnBeforeGroupSwitch : TBeforeGroupSwitchEvent read FOnBeforeGroupSwitch write FOnBeforeGroupSwitch;
  end;

  TOutlookInspectors = class;

  TNewInspectorEvent = procedure (Sender : TOutlookInspectors; const Inspector : _Inspector) of object;

  TOutlookInspectors = class(TOutlookServer)
  private
    FOnNewInspector: TNewInspectorEvent;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
  public
    //events
    property OnNewInspector : TNewInspectorEvent read FOnNewInspector write FOnNewInspector;
  end;

  TOutlookInspector = class;

  TInspectorEvent = procedure (Sender : TOutlookInspector) of object;

  TOutlookInspector = class(TOutlookServer)
  private
    FOnActivate: TInspectorEvent;
    FOnClose: TInspectorEvent;
    FOnDeactivate: TInspectorEvent;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
  public
    //events
    property OnActivate : TInspectorEvent read FOnActivate write FOnActivate;
    property OnDeactivate : TInspectorEvent read FOnDeactivate write FOnDeactivate;
    property OnClose : TInspectorEvent read FOnClose write FOnClose;
  end;

  TOutlookItem = class;

  TItemOpenEvent = procedure (Sender : TOutlookItem; var Cancel: WordBool) of object;
  TItemCustomActionEvent = procedure (Sender : TOutlookItem; const Action: IDispatch; const Response: IDispatch; var Cancel: WordBool) of object;
  TItemCustomPropertyChangeEvent = procedure (Sender : TOutlookItem; const Name: WideString) of object;
  TItemForwardEvent = procedure (Sender : TOutlookItem; const Forward: IDispatch; var Cancel: WordBool) of object;
  TItemCloseEvent = procedure (Sender : TOutlookItem; var Cancel: WordBool) of object;
  TItemPropertyChangeEvent = procedure (Sender : TOutlookItem; const Name: WideString) of object;
  TItemReadEvent = procedure (Sender : TOutlookItem) of object;
  TItemReplyEvent = procedure (Sender : TOutlookItem; const Response: IDispatch; var Cancel: WordBool) of object;
  TItemReplyAllEvent = procedure (Sender : TOutlookItem; const Response: IDispatch; var Cancel: WordBool) of object;
  TItemSendEvent = procedure (Sender : TOutlookItem; var Cancel: WordBool) of object;
  TItemWriteEvent = procedure (Sender : TOutlookItem; var Cancel: WordBool) of object;
  TItemBeforeCheckNamesEvent = procedure (Sender : TOutlookItem; var Cancel: WordBool) of object;
  TItemAttachmentAddEvent = procedure (Sender : TOutlookItem; const Attachment: Attachment) of object;
  TItemAttachmentReadEvent = procedure (Sender : TOutlookItem; const Attachment: Attachment) of object;
  TItemBeforeAttachmentSaveEvent = procedure (Sender : TOutlookItem; const Attachment: Attachment; var Cancel: WordBool) of object;

  TOutlookItem = class(TOutlookServer)
  private
    FOnOpen: TItemOpenEvent;
    FOnCustomAction: TItemCustomActionEvent;
    FOnCustomPropertyChange: TItemCustomPropertyChangeEvent;
    FOnAttachmentAdd: TItemAttachmentAddEvent;
    FOnAttachmentRead: TItemAttachmentReadEvent;
    FOnBeforeAttachmentSave: TItemBeforeAttachmentSaveEvent;
    FOnBeforeCheckNames: TItemBeforeCheckNamesEvent;
    FOnClose: TItemCloseEvent;
    FOnForward: TItemForwardEvent;
    FOnPropertyChange: TItemPropertyChangeEvent;
    FOnRead: TItemReadEvent;
    FOnReplyAll: TItemReplyAllEvent;
    FOnReply: TItemReplyEvent;
    FOnSend: TItemSendEvent;
    FOnWrite: TItemWriteEvent;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
  public
    //events
    property OnOpen : TItemOpenEvent read FOnOpen write FOnOpen;
    property OnCustomAction : TItemCustomActionEvent read FOnCustomAction write FOnCustomAction;
    property OnCustomPropertyChange : TItemCustomPropertyChangeEvent read FOnCustomPropertyChange write FOnCustomPropertyChange;
    property OnForward : TItemForwardEvent read FOnForward write FOnForward;
    property OnClose : TItemCloseEvent read FOnClose write FOnClose;
    property OnPropertyChange : TItemPropertyChangeEvent read FOnPropertyChange write FOnPropertyChange;
    property OnRead : TItemReadEvent read FOnRead write FOnRead;
    property OnReply : TItemReplyEvent read FOnReply write FOnReply;
    property OnReplyAll : TItemReplyAllEvent read FOnReplyAll write FOnReplyAll;
    property OnSend : TItemSendEvent read FOnSend write FOnSend;
    property OnWrite : TItemWriteEvent read FOnWrite write FOnWrite;
    property OnBeforeCheckNames : TItemBeforeCheckNamesEvent read FOnBeforeCheckNames write FOnBeforeCheckNames;
    property OnAttachmentAdd : TItemAttachmentAddEvent read FOnAttachmentAdd write FOnAttachmentAdd;
    property OnAttachmentRead : TItemAttachmentReadEvent read FOnAttachmentRead write FOnAttachmentRead;
    property OnBeforeAttachmentSave : TItemBeforeAttachmentSaveEvent read FOnBeforeAttachmentSave write FOnBeforeAttachmentSave;
  end;

  TOutlookCommandBarButton = class;

  TCommandBarButtonClickEvent = procedure(Sender : TOutlookCommandBarButton; const Ctrl: CommandBarButton; var CancelDefault: WordBool) of object;

  TOutlookCommandBarButton = class(TOutlookServer)
  private
    FOnClick: TCommandBarButtonClickEvent;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
  public
    property OnClick : TCommandBarButtonClickEvent read FOnClick write FOnClick;
  end;

  TOutlookExplorers = class;

  TNewExplorerEvent = procedure(Sender : TOutlookExplorers;const Explorer: _Explorer) of object;

  TOutlookExplorers = class(TOutlookServer)
  private
    FOnNewExplorer : TNewExplorerEvent;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
  public
    property OnNewExplorer : TNewExplorerEvent read FOnNewExplorer write FOnNewExplorer;
  end;

  TOutlookApplication = class;

  TApplicationItemSendEvent = procedure(Sender : TOutlookApplication; const Item: IDispatch; var Cancel: WordBool) of object;
  TReminderEvent = procedure(Sender : TOutlookApplication; const Item: IDispatch) of object;
  TOptionsPagesAddEvent = procedure(Sender : TOutlookApplication; const Pages: PropertyPages) of object;

  TOutlookApplication = class(TOutlookServer)
  private
    FOnItemSend: TApplicationItemSendEvent;
    FOnQuit: TNotifyEvent;
    FOnStartup: TNotifyEvent;
    FOnNewMail: TNotifyEvent;
    FOnOptionsPagesAdd: TOptionsPagesAddEvent;
    FOnReminder: TReminderEvent;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
  public
    property OnItemSend : TApplicationItemSendEvent read FOnItemSend write FOnItemSend;
    property OnNewMail : TNotifyEvent read FOnNewMail write FOnNewMail;
    property OnReminder : TReminderEvent read FOnReminder write FOnReminder;
    property OnOptionsPagesAdd : TOptionsPagesAddEvent read FOnOptionsPagesAdd write FOnOptionsPagesAdd;
    property OnStartup : TNotifyEvent read FOnStartup write FOnStartup;
    property OnQuit : TNotifyEvent read FOnQuit write FOnQuit;
  end;

implementation

{ TOutlookServer }

procedure TOutlookServer.Connect;
var punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk;
  end;
end;

procedure TOutlookServer.ConnectTo(svrIntf: IUnknown);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

constructor TOutlookServer.Create(svrIntf: IUnknown);
begin
  inherited Create(nil);
  AutoConnect:=false;
  ConnectKind:=ckAttachToInterface;
  ConnectTo(svrIntf);
end;

destructor TOutlookServer.Destroy;
begin
  Disconnect;
  inherited;
end;

procedure TOutlookServer.Disconnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TOutlookServer.GetConnected: boolean;
var CPC:IUnknown;
begin
  Result:=(FIntf <> nil) and Succeeded(FIntf.QueryInterface(IConnectionPointContainer, CPC))
end;

{ TOutlookExplorer }

procedure TOutlookExplorer.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{00063050-0000-0000-C000-000000000046}'; //Class_xxx
    IntfIID:   '{00063003-0000-0000-C000-000000000046}'; //IID_xxx
    EventIID:  '{0006304F-0000-0000-C000-000000000046}'; //DIID_xxx
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TOutlookExplorer.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
var Cancel : WordBool;
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    61441: if Assigned(FOnActivate) then FOnActivate(Self);
    61442: if Assigned(FOnFolderSwitch) then FOnFolderSwitch(Self);
    61444: if Assigned(FOnViewSwitch) then FOnViewSwitch(Self);
    61446: if Assigned(FOnDeactivate) then FOnDeactivate(Self);
    61447: if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
    61443: if Assigned(FOnBeforeFolderSwitch) then begin
      Cancel:=Params[1];
      FOnBeforeFolderSwitch(Self, IDispatch(Params[0]), Cancel);
      Params[1]:=Cancel;
    end;
    61445: if Assigned(FOnBeforeViewSwitch) then begin
      Cancel:=Params[1];
      FOnBeforeViewSwitch(Self, IDispatch(Params[0]), Cancel);
      Params[1]:=Cancel;
    end;
    61448: if Assigned(FOnClose) then FOnClose(Self);
  end;
end;

{ TOutlookBarPane }

procedure TOutlookBarPane.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{00063055-0000-0000-C000-000000000046}'; //Class_xxx
    IntfIID:   '{00063070-0000-0000-C000-000000000046}'; //IID_xxx
    EventIID:  '{0006307A-0000-0000-C000-000000000046}'; //DIID_xxx
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TOutlookBarPane.InvokeEvent(DispID: TDispID;
  var Params: TVariantArray);
var Cancel : WordBool;
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    61441:
      if Assigned(FOnBeforeNavigate) then begin
        Cancel:=Params[1];
        FOnBeforeNavigate(Self, IDispatch(Params[0]) as OutlookBarShortcut, Cancel);
        Params[1]:=Cancel;
      end;
    61442:
      if Assigned(FOnBeforeGroupSwitch) then begin
        Cancel:=Params[1];
        FOnBeforeGroupSwitch(Self, IDispatch(Params[0]) as OutlookBarGroup, Cancel);
        Params[1]:=Cancel;
      end;
  end;
end;

{ TInspectors }

procedure TOutlookInspectors.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{00063054-0000-0000-C000-000000000046}'; //Class_xxx
    IntfIID:   '{00063008-0000-0000-C000-000000000046}'; //IID_xxx
    EventIID:  '{00063079-0000-0000-C000-000000000046}'; //DIID_xxx
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TOutlookInspectors.InvokeEvent(DispID: TDispID;
  var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    61441: if Assigned(FOnNewInspector) then FOnNewInspector(Self, IDispatch(Params[0]) as _Inspector);
  end;
end;

{ TOutlookInspector }

procedure TOutlookInspector.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{00063058-0000-0000-C000-000000000046}'; //Class_xxx
    IntfIID:   '{00063005-0000-0000-C000-000000000046}'; //IID_xxx
    EventIID:  '{0006307D-0000-0000-C000-000000000046}'; //DIID_xxx
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TOutlookInspector.InvokeEvent(DispID: TDispID;
  var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    61441: if Assigned(FOnActivate) then FOnActivate(Self);
    61446: if Assigned(FOnDeactivate) then FOnDeactivate(Self);
    61448: if Assigned(FOnClose) then FOnClose(Self);
  end;
end;

{ TOutlookItem }

procedure TOutlookItem.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{00061033-0000-0000-C000-000000000046}'; //Class_xxx
    IntfIID:   '{00063034-0000-0000-C000-000000000046}'; //IID_xxx
    EventIID:  '{0006303A-0000-0000-C000-000000000046}'; //DIID_xxx
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TOutlookItem.InvokeEvent(DispID: TDispID;
  var Params: TVariantArray);
var Cancel:WordBool;
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    61443: if Assigned(FOnOpen) then begin
      Cancel:=Params[0];
      FOnOpen(Self, Cancel);
      Params[0]:=Cancel;
    end;
    61446: if Assigned(FOnCustomAction) then begin
      Cancel:=Params[2];
      FOnCustomAction(Self, IDispatch(Params[0]), IDispatch(Params[1]), Cancel);
      Params[2]:=Cancel;
    end;
    61448 : if Assigned(FOnCustomPropertyChange) then FOnCustomPropertyChange(Self, Params[0]);
    62568: if Assigned(FOnForward) then begin
      Cancel:=Params[1];
      FOnForward(Self, IDispatch(Params[0]), Cancel);
      Params[1]:=Cancel;
    end;
    61444: if Assigned(FOnClose) then begin
      Cancel:=Params[0];
      FOnClose(Self, Cancel);
      Params[0]:=Cancel;
    end;
    61449 : if Assigned(FOnPropertyChange) then FOnPropertyChange(Self, Params[0]);
    61441 : if Assigned(FOnRead) then FOnRead(Self);
    62566: if Assigned(FOnReply) then begin
      Cancel:=Params[1];
      FOnReply(Self, IDispatch(Params[0]), Cancel);
      Params[1]:=Cancel;
    end;
    62567: if Assigned(FOnReplyAll) then begin
      Cancel:=Params[1];
      FOnReplyAll(Self, IDispatch(Params[0]), Cancel);
      Params[1]:=Cancel;
    end;
    61445: if Assigned(FOnSend) then begin
      Cancel:=Params[0];
      FOnSend(Self, Cancel);
      Params[0]:=Cancel;
    end;
    61442: if Assigned(FOnWrite) then begin
      Cancel:=Params[0];
      FOnWrite(Self, Cancel);
      Params[0]:=Cancel;
    end;
    61450: if Assigned(FOnBeforeCheckNames) then begin
      Cancel:=Params[0];
      FOnBeforeCheckNames(Self, Cancel);
      Params[0]:=Cancel;
    end;
    61453: if Assigned(FOnBeforeAttachmentSave) then begin
      Cancel:=Params[1];
      FOnBeforeAttachmentSave(Self, Attachment(IDispatch(Params[0])), Cancel);
      Params[1]:=Cancel;
    end;
    61451 : if Assigned(FOnAttachmentAdd) then FOnAttachmentAdd(Self,  Attachment(IDispatch(Params[0])));
    61452 : if Assigned(FOnAttachmentRead) then FOnAttachmentRead(Self,  Attachment(IDispatch(Params[0])));
    else begin
      sleep(0);
    end;
  end;
end;

{ TOutlookCommandBarButton }

procedure TOutlookCommandBarButton.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{55F88891-7708-11D1-ACEB-006008961DA5}'; //Class_xxx
    IntfIID:   '{000C030E-0000-0000-C000-000000000046}'; //IID_xxx
    EventIID:  '{000C0351-0000-0000-C000-000000000046}'; //DIID_xxx
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TOutlookCommandBarButton.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
var Cancel:WordBool;
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    1: if Assigned(FOnClick) then begin
      Cancel:=Params[1];
      FOnClick(Self, IDispatch(Params[0]) as _CommandBarButton, Cancel);
      Params[1]:=Cancel;
    end;
  end;
end;

{ TOutlookExplorers }

procedure TOutlookExplorers.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{00063053-0000-0000-C000-000000000046}'; //Class_xxx
    IntfIID:   '{0006300A-0000-0000-C000-000000000046}'; //IID_xxx
    EventIID:  '{00063078-0000-0000-C000-000000000046}'; //DIID_xxx
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TOutlookExplorers.InvokeEvent(DispID: TDispID;
  var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    61441: if Assigned(FOnNewExplorer) then begin
      FOnNewExplorer(Self, IDispatch(Params[0]) as _Explorer);
    end;
  end;
end;

{ TOutlookApplication }

procedure TOutlookApplication.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{0006F03A-0000-0000-C000-000000000046}'; //Class_xxx
    IntfIID:   '{00063001-0000-0000-C000-000000000046}'; //IID_xxx
    EventIID:  '{0006304E-0000-0000-C000-000000000046}'; //DIID_xxx
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TOutlookApplication.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
var Cancel:WordBool;
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    61442 : if Assigned(FOnItemSend) then begin
              Cancel:=Params[1];
              FOnItemSend(Self, Params[0],Cancel);
              Params[1]:=Cancel;
            end;
    61443 : if Assigned(FOnNewMail) then FOnNewMail(Self);
    61444 : if Assigned(FOnReminder) then FOnReminder(Self, Params[0]);
    61445 : if Assigned(FOnOptionsPagesAdd) then FOnOptionsPagesAdd(Self, IUnknown(Params[0]) as PropertyPages);
    61446 : if Assigned(FOnStartup) then FOnStartup(Self);
    61447 : if Assigned(FOnQuit) then FOnQuit(Self);
  end;
end;

end.
