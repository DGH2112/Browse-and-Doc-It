(**

  This is a debug form for displaying the tokens and their information.

  @version      1.0
  @date         14 Oct 2018
  @author       David Hoyle

**)
unit BADI.TokenForm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  BADI.Base.Module;

type
  (** This class represents a forms for inspecting the token in a module. **)
  TfrmTokenForm = class(TForm)
    lvListView1: TListView;
    procedure lvListView1CustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Class Procedure Execute(Source : TBaseLanguageModule);
  end;

implementation

Uses
  ProgressForm, BADI.Types, BADI.Options, BADI.Constants;

{$R *.DFM}

(**

  This is a custom draw method for the TListView component. It colours the
  items differently depending on the contents of the item.caption property.

  @precon  Sender is the object initiating the event, Item is the item in the
           list view being displayed, State is the state of the item being
           displayed and DefaultDraw can be changed to to define default drawing
           or owner drawing.
  @postcon This method colours the items in the list view based on their type.

  @param   Sender      as a TCustomListView
  @param   Item        as a TListItem
  @param   State       as a TCustomDrawState
  @param   DefaultDraw as a Boolean as a reference

**)
procedure TfrmTokenForm.lvListView1CustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);

Var
  T : TBADITokenType;
  BG: TColor;
  TokenFontInfo : TBADITokenFontInfoTokenSet;

begin
  T := TBADITokenType(StrToInt(Item.SubItems[6]));
  TokenFontInfo := TBADIOptions.BADIOptions.TokenFontInfo[TBADIOptions.BADIOptions.UseIDEEditorColours];
  lvListView1.Canvas.Font.Color := TokenFontInfo[T].FForeColour;
  lvListView1.Canvas.Font.Style := TokenFontInfo[T].FStyles;
  BG := TokenFontInfo[T].FBackColour;
  If BG = clNone Then
    BG := clWindow;
  lvListView1.Canvas.Brush.Color := BG;
end;

(**

  This is the forms OnDestroy event. It clears the contents of the list view.

  @precon  Sender is the object initiating the event.
  @postcon Cleans up the list view.

  @param   Sender as a TObject

**)
procedure TfrmTokenForm.FormDestroy(Sender: TObject);
begin
  lvListView1.Items.BeginUpdate;
  lvListView1.Items.Clear;
end;

(**

  This is a class method used to invokes this token dialogue.

  @precon  Source is a valid instance of a TPsacalDocModule that requires its
           tokens displaying.
  @postcon Displays the token form.

  @param   Source as a TBaseLanguageModule

**)
class procedure TfrmTokenForm.Execute(Source: TBaseLanguageModule);

Var
  i : Integer;
  frm : TfrmProgress;
  Item : TListItem;

begin
  With TfrmTokenForm.Create(nil) Do
    Try
      frm := TfrmProgress.Create(Nil);
      Try
        frm.Init(Source.TokenCount, 'Show Tokens', 'Getting Tokens...');
        lvListView1.Items.BeginUpdate;
        Try
          For i := 0 To Source.TokenCount - 1 Do
            Begin
              Item := lvListView1.Items.Add;
              Item.Caption := IntToStr(i);
              Item.SubItems.Add(strTokenType[Source.Tokens[i].TokenType]);
              Item.SubItems.Add(IntToStr(Source.Tokens[i].BufferPos));
              Item.SubItems.Add(IntToStr(Source.Tokens[i].Line));
              Item.SubItems.Add(IntToStr(Source.Tokens[i].Column));
              Item.SubItems.Add(IntToStr(Source.Tokens[i].Length));
              Item.SubItems.Add(Source.Tokens[i].Token);
              Item.SubItems.Add(Format('%d', [Integer(Source.Tokens[i].TokenType)]));
              If i Mod 10 = 0 Then
                frm.UpdateProgress(i, Format('Getting Tokens... %d', [i]));
            End;
        Finally
          lvListView1.Items.EndUpdate;
        End;
      Finally
        frm.Free;
      End;
      ShowModal;
    Finally
      Free;
    End;
end;

end.
