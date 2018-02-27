unit fTranslate;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls, ComCtrls, uParentedWnd, ShellAPI;

type
  TdlgTranslate = class(TParentedForm)
    CloseBtn: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    OriginalTxt: TRichEdit;
    TranslationTxt: TRichEdit;
    Label2: TLabel;
    imAltaVista: TImage;
    imSysTran: TImage;
    procedure imAltaVistaClick(Sender: TObject);
    procedure imSysTranClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TdlgTranslate.imAltaVistaClick(Sender: TObject);
begin
  ShellExecute(GetForegroundWindow, 'open', 'http://babelfish.altavista.com', nil, nil, SW_SHOW);
end;

procedure TdlgTranslate.imSysTranClick(Sender: TObject);
begin
  ShellExecute(GetForegroundWindow, 'open', 'http://www.systransoft.com', nil, nil, SW_SHOW);
end;

procedure TdlgTranslate.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Escape then begin
    ModalResult:=mrCancel;
  end;
end;

end.
