(**

  This is a debug form for displaying the tokens and their information.

  @Author  David Hoyle
  @Version 1.096
  @Date    28 Aug 2020

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
  Strict Private
  Strict Protected
  Public
    Class Procedure Execute(Const Source : TBaseLanguageModule);
  End;

implementation

Uses
  BADI.ProgressForm,
  BADI.Types,
  BADI.Options,
  BADI.Constants;

{$R *.DFM}

(**

  This is a class method used to invokes this token dialogue.

  @precon  Source is a valid instance of a TBaseLanguageModule that requires its tokens displaying.
  @postcon Displays the token form.

  @param   Source as a TBaseLanguageModule as a constant

**)
class procedure TfrmTokenForm.Execute(Const Source: TBaseLanguageModule);

ResourceString
  strShowTokens = 'Show Tokens';
  strGettingTokens = 'Getting Tokens...';
  strGettingTokensD = 'Getting Tokens... %d';

Const
  iUpdateInterval = 10;

Var
  F: TfrmTokenForm;
  i : Integer;
  frm : TfrmProgress;
  Item : TListItem;

begin
  F := TfrmTokenForm.Create(Application.MainForm);
  Try
    frm := TfrmProgress.Create(Nil);
    Try
      frm.Init(Source.TokenCount, strShowTokens, strGettingTokens);
      F.lvListView1.Items.BeginUpdate;
      Try
        For i := 0 To Source.TokenCount - 1 Do
          Begin
            Item := F.lvListView1.Items.Add;
            Item.Caption := IntToStr(i);
            Item.SubItems.Add(strTokenType[Source.Tokens[i].TokenType]);
            Item.SubItems.Add(IntToStr(Source.Tokens[i].BufferPos));
            Item.SubItems.Add(IntToStr(Source.Tokens[i].Line));
            Item.SubItems.Add(IntToStr(Source.Tokens[i].Column));
            Item.SubItems.Add(IntToStr(Source.Tokens[i].Length));
            Item.SubItems.Add(Source.Tokens[i].Token);
            Item.SubItems.Add(Format('%d', [Integer(Source.Tokens[i].TokenType)]));
            If i Mod iUpdateInterval = 0 Then
              frm.UpdateProgress(i, Format(strGettingTokensD, [i]));
          End;
      Finally
        F.lvListView1.Items.EndUpdate;
      End;
    Finally
      frm.Free;
    End;
    F.ShowModal;
  Finally
    F.Free;
  End;
end;

(**

  This is the forms On Destroy event. It clears the contents of the list view.

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

  This is a custom draw method for the List View component. It colours the
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
Procedure TfrmTokenForm.lvListView1CustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Var DefaultDraw: Boolean);

Const
  iTokenTypeIndex = 6;

Var
  T: TBADITokenType;
  BG: TColor;
  TokenFontInfo: TBADITokenFontInfoTokenSet;

Begin
  T := TBADITokenType(StrToInt(Item.SubItems[iTokenTypeIndex]));
  TokenFontInfo := TBADIOptions.BADIOptions.TokenFontInfo[TBADIOptions.BADIOptions.UseIDEEditorColours];
  lvListView1.Canvas.Font.Color := TokenFontInfo[T].FForeColour;
  lvListView1.Canvas.Font.Style := TokenFontInfo[T].FStyles;
  BG := TokenFontInfo[T].FBackColour;
  If BG = clNone Then
    BG := clWindow;
  lvListView1.Canvas.Brush.Color := BG;
End;

End.
