(**

  This module contains a class which represent a form for defining the option
  for Documentation.

  @Version 1.051
  @Author  David Hoyle
  @Date    02 May 2021

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
unit BADI.DocumentationOptionsForm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  ExtCtrls,
  BADI.Documentation.Dispatcher;

type
  (** A class to represent the form interface. **)
  TfrmDocumentationOptions = class(TForm)
    rgpDocumentationOptions: TRadioGroup;
    gbxScopeOptions: TGroupBox;
    chkLocal: TCheckBox;
    chkPrivate: TCheckBox;
    chkProtected: TCheckBox;
    chkPublic: TCheckBox;
    chkPublished: TCheckBox;
    lblCSSComment: TLabel;
    btnOK: TButton;
    ilButtons: TImageList;
    btnCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
    Class Function Execute(var ADocType : TDocType) : Boolean;
  end;

implementation

Uses
  {$IFNDEF STANDALONEAPP}
  BADI.ToolsAPIUtils,
  {$ENDIF}
  BADI.Base.Module,
  BADI.Types,
  BADI.Options;

{$R *.dfm}

(**

  This is the forms main interface method for invoking the dialogue.

  @precon  None.
  @postcon Returns the documentation type require in the var parameter if the dialogue is accepted while
           returning true, else returns false.

  @param   ADocType as a TDocType as a reference
  @return  a Boolean

**)
class function TfrmDocumentationOptions.Execute(var ADocType: TDocType): Boolean;

Var
  i : TDocType;
  F: TfrmDocumentationOptions;

begin
  Result := False;
  F := TfrmDocumentationOptions.Create(Nil);
  Try
    {$IFNDEF STANDALONEAPP}
    TBADIToolsAPIFunctions.RegisterFormClassForTheming(TfrmDocumentationOptions, F);
    {$ENDIF}
    For i := Low(TDocType) to High(TDocType) Do
      F.rgpDocumentationOptions.Items.Add(strDocumentationTypes[i]);
    F.rgpDocumentationOptions.ItemIndex := Byte(ADocType);
    F.chkLocal.Checked := scLocal In TBADIOptions.BADIOptions.ScopesToDocument;
    F.chkPrivate.Checked := scPrivate In TBADIOptions.BADIOptions.ScopesToDocument;
    F.chkProtected.Checked := scProtected In TBADIOptions.BADIOptions.ScopesToDocument;
    F.chkPublic.Checked := scPublic In TBADIOptions.BADIOptions.ScopesToDocument;
    F.chkPublished.Checked := scPublished In TBADIOptions.BADIOptions.ScopesToDocument;
    If F.ShowModal = mrOK Then
      Begin
        ADocType := TDocType(F.rgpDocumentationOptions.ItemIndex);
        TBADIOptions.BADIOptions.ScopesToDocument := [];
        If F.chkLocal.Checked Then
          TBADIOptions.BADIOptions.ScopesToDocument :=
            TBADIOptions.BADIOptions.ScopesToDocument + [scLocal];
        If F.chkPrivate.Checked Then
          TBADIOptions.BADIOptions.ScopesToDocument :=
            TBADIOptions.BADIOptions.ScopesToDocument + [scPrivate];
        If F.chkProtected.Checked Then
          TBADIOptions.BADIOptions.ScopesToDocument :=
            TBADIOptions.BADIOptions.ScopesToDocument + [scProtected];
        If F.chkPublic.Checked Then
          TBADIOptions.BADIOptions.ScopesToDocument :=
            TBADIOptions.BADIOptions.ScopesToDocument + [scPublic];
        If F.chkPublished.Checked Then
          TBADIOptions.BADIOptions.ScopesToDocument :=
            TBADIOptions.BADIOptions.ScopesToDocument + [scPublished];
        Result := True;
      End;
  Finally
    F.Free;
  End;
end;

end.
