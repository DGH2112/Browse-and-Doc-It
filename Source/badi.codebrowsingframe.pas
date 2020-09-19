(**

  This module contains a class to represent a frame interface for the code browsing
  options.

  @Author  David Hoyle
  @Version 1.001
  @Date    19 Sep 2020

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
Unit BADI.CodeBrowsingFrame;

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
  ExtCtrls,
  BADI.CustomOptionsFrame;

Type
  (** A class to represent the frame interface. **)
  TfmBADICodeBrowsingFrame = Class(TFrame, IBADIOptionsFrame)
    rgpBrowsePosition: TRadioGroup;
  Private
    { Private declarations }
  Public
    { Public declarations }
    Procedure LoadSettings;
    Procedure SaveSettings;
  End;

Implementation

{$R *.dfm}

Uses
  BADI.Base.Module,
  BADI.Options,
  BADI.Types;

{ TfmBADICodeBrowsingFrame }

(**

  This method loads the code browsing information from the options.

  @precon  None.
  @postcon loads the code browsing information from the options.

**)
Procedure TfmBADICodeBrowsingFrame.LoadSettings;

Begin
  rgpBrowsePosition.ItemIndex := Integer(TBADIOptions.BADIOptions.BrowsePosition);
End;

(**

  This method saves the code browsing information to the options.

  @precon  None.
  @postcon Saves the code browsing information to the options.

**)
Procedure TfmBADICodeBrowsingFrame.SaveSettings;

Begin
  TBADIOptions.BADIOptions.BrowsePosition := TBrowsePosition(rgpBrowsePosition.ItemIndex);
End;

End.
