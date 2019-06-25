(**

  This module contains code for creating the about box entry in RAD Studio.

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
Unit BADI.AboutBox;

Interface

{$INCLUDE CompilerDefinitions.inc}

  Procedure AddAboutBoxEntry;
  Procedure RemoveAboutBoxEntry;

Implementation

Uses
  ToolsAPI,
  SysUtils,
  Windows,
  BADI.Functions,
  Forms,
  BADI.Constants;

{$IFDEF D2005}
Var
  (** An index for the About Box Plugin. - required for unloading the interface. **)
  iAboutPlugin : Integer;
{$ENDIF}

(**

  This procedure adds the about box entry to the RAD Studio IDE.

  @precon  None.
  @postcon If the version of RAD Studio is equal to or above 2005 the about box entry is
           added to the IDE.

**)
Procedure AddAboutBoxEntry;

Const
  strBrowseAndDocItSplashScreenBitMap = 'BrowseAndDocItSplashScreenBitMap48x48';
  strSKUBuild = 'SKU Build %d.%d.%d.%d';

ResourceString
  strIDEExpertToBrowseAndDocumentYourSourceCode = 'An IDE expert to browse and document your source ' + 
    'code.';

Var
  iMajor : Integer;
  iMinor : Integer;
  iBugFix : Integer;
  iBuild : Integer;
  bmSplashScreen : HBITMAP;

Begin
  {$IFDEF D2005}
  BuildNumber(iMajor, iMinor, iBugFix, iBuild);
  bmSplashScreen := LoadBitmap(hInstance, strBrowseAndDocItSplashScreenBitMap);
  iAboutPlugin := (BorlandIDEServices As IOTAAboutBoxServices).AddPluginInfo(
    Format(strSplashScreenName, [iMajor, iMinor, Copy(strRevision, iBugFix + 1, 1),
      Application.Title]),
    strIDEExpertToBrowseAndDocumentYourSourceCode,
    bmSplashScreen,
    {$IFDEF DEBUG} True {$ELSE} False {$ENDIF},
    Format(strSplashScreenBuild, [iMajor, iMinor, iBugfix, iBuild]),
    Format(strSKUBuild, [iMajor, iMinor, iBugfix, iBuild]));
  {$ENDIF}
End;

(**

  This procedure removes the about box entry from the IDE.

  @precon  None.
  @postcon The about box entry is removed from the IDE.

**)
Procedure RemoveAboutBoxEntry;

Begin
  {$IFDEF D2010}
  If iAboutPlugin > iWizardFailState Then
    (BorlandIDEServices As IOTAAboutBoxServices).RemovePluginInfo(iAboutPlugin);
  {$ENDIF}
End;

End.
