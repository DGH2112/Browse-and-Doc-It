unit fPropetyPage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, BFOutlook_TLB, StdVcl, Outlook_TLB, StdCtrls, ExtCtrls,
  ShellAPI;

type
  TToolsOptionsPage = class(TActiveForm, IToolsOptionsPage, PropertyPage)
    imAltaVista: TImage;
    imSysTran: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Image1: TImage;
    Label5: TLabel;
    procedure GoAltaVista(Sender: TObject);
    procedure GoSysTran(Sender: TObject);
    procedure GoBorland(Sender: TObject);
    procedure GoDimastrCom(Sender: TObject);
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Initialize; override;
    destructor Destroy;override; 
    //PropertyPage
    function  GetPageInfo(var HelpFile: WideString; var HelpContext: Integer): HResult; stdcall;
    function  Get_Dirty(out Dirty: WordBool): HResult; stdcall;
    function  Apply: HResult; stdcall;
  end;

implementation

uses ComObj, ComServ;

{$R *.DFM}

{ TActiveFormX }

function TToolsOptionsPage.Apply: HResult;
begin
  Result:=S_OK;
end;

function TToolsOptionsPage.Get_Dirty(out Dirty: WordBool): HResult;
begin
  Result:=S_OK;
end;

function TToolsOptionsPage.GetPageInfo(var HelpFile: WideString;
  var HelpContext: Integer): HResult;
begin
  Result:=S_OK;
end;

procedure TToolsOptionsPage.Initialize;
begin
  inherited Initialize;
end;

procedure TToolsOptionsPage.GoAltaVista(Sender: TObject);
begin
  ShellExecute(GetForegroundWindow, 'open', 'http://babelfish.altavista.com', nil, nil, SW_SHOW);
end;

procedure TToolsOptionsPage.GoSysTran(Sender: TObject);
begin
  ShellExecute(GetForegroundWindow, 'open', 'http://www.systransoft.com', nil, nil, SW_SHOW);
end;

procedure TToolsOptionsPage.GoBorland(Sender: TObject);
begin
  ShellExecute(GetForegroundWindow, 'open', 'http://www.borland.com/delphi/', nil, nil, SW_SHOW);
end;

procedure TToolsOptionsPage.GoDimastrCom(Sender: TObject);
begin
  ShellExecute(GetForegroundWindow, 'open', 'http://www.dimastr.com/babelfish/?Referrer=OutlookOptions', nil, nil, SW_SHOW);
end;

destructor TToolsOptionsPage.Destroy;
begin
  inherited;
end;

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TToolsOptionsPage,
    Class_ToolsOptionsPage,
    2,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
