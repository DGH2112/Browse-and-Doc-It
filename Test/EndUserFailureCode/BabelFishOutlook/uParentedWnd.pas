unit uParentedWnd;

interface

uses Windows, Messages, Classes, Controls, Forms;

type

  TParentedForm = class(TForm)
  private
    FWndParent : HWND;
    FFreeOnClose: boolean;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FormClose(Sender: TObject;  var Action: TCloseAction); virtual;
  public
    constructor Create(AOwner : TComponent; AWndParent : HWND = 0); reintroduce; virtual;
    constructor CreateNew(AOwner : TComponent; AWndParent : HWND = 0); reintroduce; virtual;
    destructor Destroy;override;
    property WndParent : HWND read FWndParent;
    property FreeOnClose : boolean read FFreeOnClose write FFreeOnClose;
  end;

implementation

var gHook : HHOOK=0;
    ParentedWindowsList : TList;

function HookProc(Code : integer; wParam : WPARAM; lParam : LPARAM):LResult;stdcall;
var pCWP : PCWPSTRUCT;
    i : integer;
begin
  //call first, while gOutlookHook is still valid
  Result:=CallNextHookEx(gHook, Code, wParam, lParam);
  //do we need to bail out?
  if Code = HC_ACTION then begin
    pCWP:=pointer(lParam);
    if (pCWP <> nil) and ((pCWP.message = WM_DESTROY) or (pCWP.message = WM_CLOSE)) then begin
      for i:=ParentedWindowsList.Count-1 downto 0 do begin
        if TParentedForm(ParentedWindowsList[i]).WndParent = pCWP.hwnd then begin
          TParentedForm(ParentedWindowsList[i]).Free; //it will delete itself from the list
        end;
      end;
    end;
  end;
end;

{ TParentedForm }

constructor TParentedForm.Create(AOwner: TComponent; AWndParent: HWND);
begin
  FFreeOnClose:=false;//true;
  ParentedWindowsList.Add(Self);
  FWndParent:=AWndParent; //do it before calling the inherited constructor!!!
  inherited Create(AOwner);
  OnClose:=FormClose;
end;

constructor TParentedForm.CreateNew(AOwner: TComponent; AWndParent: HWND);
begin
  ParentedWindowsList.Add(Self);
  FWndParent:=AWndParent; //do it before calling the inherited constructor!!!
  inherited CreateNew(Application);
  OnClose:=FormClose;
end;

procedure TParentedForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if FWndParent <> 0 then Params.WndParent:=FWndParent; //defaults to Application.Handle in inherited CreateParams
end;

destructor TParentedForm.Destroy;
var OldWnd : HWND;
    ind : integer;
begin
  ind := ParentedWindowsList.IndexOf(Self);
  if ind >=0 then ParentedWindowsList.Delete(ind);
  OldWnd:=Application.Handle;
  Application.Handle:=0;
  inherited;
  Application.Handle:=OldWnd;
end;

procedure TParentedForm.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
  if FFreeOnClose then begin
    Visible:=false;
    //Action:=caFree;
    //Action:=caHide;
    Release;
  end;
end;

initialization
  ParentedWindowsList := TList.Create;
  gHook:=SetWindowsHookEx(WH_CALLWNDPROC, HookProc, 0, GetCurrentThreadId());
finalization
  UnhookWindowsHookEx(gHook);
  gHook:=0;
  ParentedWindowsList.Free;
end.
