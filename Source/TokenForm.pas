(**

  This is a debug form for displaying the tokens and their information.

  @version      0.9
  @date         25 May 2006
  @author       David Hoyle

**)
unit TokenForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, BaseLanguageModule, PascalDocModule;

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
    Class Procedure Execute(Source : TPascalDocModule);
  end;

implementation

Uses
  ProgressForm;

{$R *.DFM}

(**

  This function returns the colour requires correspondig to the type of token
  in strText.

  @precon  strText is a string representation of a token type.
  @postcon Returns the appropriate colour for the token type.
  
  @param   strText as a String
  @return  a TColor

**)
Function GetColour(strText : String) : TColor;

Var
  i : TTokenType;

Begin
  Result := clBlack;
  For i := ttUnknown To ttDirective Do
    If strText = strTypes[i] Then
      Case i Of
        ttUnKnown : Result := clRed;
        ttReservedWord, ttDirective : Result := clBlack;
        ttIdentifier : Result := clNavy;
        ttSymbol, ttNumber : Result := clGreen;
        ttStringLiteral : Result := clTeal;
        ttComment, ttCompilerDirective : Result := clPurple;
        ttHTMLTag : Result := clBlue;
      Else
        Result := clBlack;
      End;
End;

(**

  This method returns the requires font style for the type of token.

  @precon  strText is a text representation of a token type.
  @postcon Returns a font style set for the given token type.

  @param   strText as a String
  @return  a TFontStyles

**)
Function GetStyle(strText : String) : TFontStyles;

Var
  i : TTokenType;

Begin
  Result := [];
  For i := ttUnknown To ttDirective Do
    If strText = strTypes[i] Then
      Case i Of
        ttUnKnown : Result := [];
        ttReservedWord, ttDirective : Result := [fsBold];
        ttIdentifier : Result := [];
        ttSymbol, ttNumber : Result := [fsBold];
        ttStringLiteral : Result := [fsBold];
        ttComment, ttCompilerDirective : Result := [fsItalic];
        ttHTMLTag : Result := [fsBold];
      Else
        Result := [];
      End;
End;

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
begin
  lvListView1.Canvas.Font.Color := GetColour(Item.SubItems[0]);
  lvListView1.Canvas.Font.Style := GetStyle(Item.SubItems[0]);
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
           
  @param   Source as a TPascalDocModule

**)
class procedure TfrmTokenForm.Execute(Source: TPascalDocModule);

Var
  i : Integer;
  frm : TfrmProgress;

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
              With lvListView1.Items.Add Do
                Begin
                  Caption := IntToStr(i);
                  SubItems.Add(strTypes[Source.TokenInfo[i].TokenType]);
                  SubItems.Add(IntToStr(Source.TokenInfo[i].BufferPos));
                  SubItems.Add(IntToStr(Source.TokenInfo[i].Line));
                  SubItems.Add(IntToStr(Source.TokenInfo[i].Column));
                  SubItems.Add(IntToStr(Source.TokenInfo[i].Length));
                  SubItems.Add(Source.TokenInfo[i].Token);
                End;
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
