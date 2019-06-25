(**

  This module contains a class to add and edit method descriptions.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Jun 2019

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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
Unit BADI.MethodDescriptionForm;

Interface

Uses
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
  Vcl.ExtCtrls;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class to represent the form interface. **)
  TfrmMethodDescriptions = Class(TForm)
    lblPattern: TLabel;
    edtPattern: TEdit;
    lblDescription: TLabel;
    edtDescription: TEdit;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pnlForm: TPanel;
  Strict Private
  Strict Protected
  Public
    Class Function Execute(Var strPattern, strDescription: String): Boolean;
  End;

Implementation

{$R *.dfm}

{$IFNDEF STANDALONEAPP}
Uses
  ToolsAPI,
  BADI.ToolsAPIUtils;
{$ENDIF}

(**


  This is the forms main interface method for getting the pattern and
  description.

  @precon  None.
  @postcon If the dialogue is confirmed then the pattern and description are
           returned in the var parameters and the function retuen true.


  @param   strPattern     as a String as a reference
  @param   strDescription as a String as a reference
  @return  a Boolean

**)
Class Function TfrmMethodDescriptions.Execute(Var strPattern, strDescription: String): Boolean;

Var
  F : TfrmMethodDescriptions;
  
Begin
  Result := False;
  { $IFDEF DXE102
  TBADIToolsAPIFunctions.RegisterFormClassForTheming(TfrmMethodDescriptions);
  TBADIToolsAPIFunctions.ApplyTheming(Self);
  {$ENDIF}
  F := TfrmMethodDescriptions.Create(Nil);
  Try
    { $IFDEF DXE102
    If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
      If ITS.IDEThemingEnabled Then
        ITS.ApplyTheme(F);
    {$ENDIF}
    F.edtPattern.Text := strPattern;
    F.edtDescription.Text := strDescription;
    If F.ShowModal = mrOK Then
      Begin
        strPattern := F.edtPattern.Text;
        strDescription := F.edtDescription.Text;
        Result := True;
      End;
  Finally
    F.Free;
  End;
End;

End.
