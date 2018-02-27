unit dlgWait;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls, ComCtrls, ActiveX, SoapHTTPClient, Messages, Dialogs,
  uParentedWnd, uBabelfishSOAP;

type

  TWaitDlg = class;

  TBabelFishThread = class(TThread)
  private
    FOwner : TWaitDlg;
  protected
    procedure Execute;override;
  public
    constructor Create(Owner : TWaitDlg);reintroduce;
  end;

  TWaitDlg = class(TParentedForm)
    CancelBtn: TButton;
    Bevel1: TBevel;
    Animate1: TAnimate;
    Timer: TTimer;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
    FInput : string;
    FResult : string;
    FThread : TBabelFishThread;
    FException : string;
    FLanguage : string;
    FHTTPRIO : THTTPRIO;
    procedure OnThreadTerminate(Sender : TObject);
  public
    { Public declarations }
    function Translate(Text : string; Language : string) : string; //can raise an exception!
  end;


implementation

function DecodeUTF8(const Value : string):string;
var i, j : integer;
    N : integer;
    HugeChar : ULONG; //4 bytes
begin
  Result:='';
  i:=1;
  while i < Length(Value) do begin
    if byte(Value[i]) < $80 then begin
      Result:=Result+Value[i]; //no change required
      i:=i+1;
    end
    else begin
      //find out the number of bytes used for this character
      N:=0;
      for j:=1 to 8 do begin
      //start with the highest bit and cound the bumber
      //of "1" before "0"
        if (byte(Value[i]) and (1 shl (8-j))) = 0 then Break;
        inc(N);
      end;
      //ShowMessage('N:'+IntToStr(N));
      HugeChar:=byte(Value[i]) and ($FF shr (N+1));
      //ShowMessage('HugeChar:'+IntToStr(HugeChar));
      for j:=1 to N-1 do begin
        HugeChar:=(HugeChar shl 6) or byte(byte(Value[i+j]) and $3F);
      end;
      //ShowMessage('HugeChar:'+IntToStr(HugeChar));
      Result:=Result+char(HugeChar);
      i:=i+N;
    end;
  end;
end;

//only work on bytes 0..255
function EncodeUTF8(const Value : string):string;
var i : integer;
begin
  for i:=1 to Length(Value) do begin
    if byte(Value[i]) < $80 then begin
      Result:=Result+Value[i]; //no change required
    end
    else begin
      Result:=Result+char($C0{11000000} or (byte(Value[i]) shr 6))+
                     char($80{10000000} or (byte(Value[i]) and $3F{111111}));
    end;
  end;
end;

{$R *.DFM}

{ TWaitDlg }

function TWaitDlg.Translate(Text, Language: string): string;
begin
  Result:='';
  FInput:=Text;
  FLanguage:=Language;
  FException:='';
  FResult:='';
  if ShowModal = mrOk then begin
    Result:=FResult;
  end
  else if Length(FException) > 0 then begin
    raise Exception.Create(FException);
  end;
end;

procedure TWaitDlg.OnThreadTerminate(Sender: TObject);
begin
  if Length(FException) = 0
    then begin
      ModalResult:=mrOk;
    end
    else begin
      ModalResult:=mrCancel;
    end;
  //PostMessage(Handle, WM_CLOSE, 0, 0);
  //Close;
end;

procedure TWaitDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FThread);
end;

procedure TWaitDlg.FormCreate(Sender: TObject);
begin
  FThread := nil;
end;

procedure TWaitDlg.FormShow(Sender: TObject);
begin
  FThread := TBabelFishThread.Create(Self);
  FThread.OnTerminate:=OnThreadTerminate;
  FThread.Resume;
end;

{ TBabelFishThread }

constructor TBabelFishThread.Create(Owner: TWaitDlg);
begin
  inherited Create(true);
  FOwner:=Owner;
end;

procedure TBabelFishThread.Execute;
begin
  try
    CoInitialize(nil);
    try
      FOwner.FHTTPRIO:=THTTPRIO.Create(nil);
      try
        FOwner.FHTTPRIO.WSDLLocation := 'http://www.xmethods.net/sd/BabelFishService.wsdl';
        FOwner.FHTTPRIO.Service := 'BabelFish';
        FOwner.FHTTPRIO.Port := 'BabelFishPort';
        FOwner.FResult:=DecodeUTF8(
           (FOwner.FHTTPRIO as BabelFishPortType).BabelFish(FOwner.FLanguage, EncodeUTF8(FOwner.FInput))
          );
      finally
        FOwner.FHTTPRIO.Free;
        FOwner.FHTTPRIO:=nil;
      end;
      //grab FOwner.FInput
      //Sleep(5000);
      //raise Exception.Create('oops...');
      //FOwner.FResult:='test result';
    finally
      CoUninitialize;
    end;
  except
    on E:Exception do FOwner.FException:=E.Message;
  end;
end;


procedure TWaitDlg.CancelBtnClick(Sender: TObject);
begin
  //FOwner.FHTTPRIO.HTTPWebNode.
end;

procedure TWaitDlg.TimerTimer(Sender: TObject);
begin
  //do nothing, we just need to keep messages coming so that the dialog will
  //be able to get of the modal loop even if there are no messages
end;

end.
